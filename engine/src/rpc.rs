use bevy::ecs::system::Resource;
use rmp_serde::decode::{Deserializer, ReadReader};
use rmp_serde::{decode, encode};
use serde::de;
use serde::de::{DeserializeSeed, SeqAccess, Visitor};
use serde::ser::{SerializeTuple, Serializer};
use serde::Serialize;
use std::fmt;
use std::io::Write;

#[derive(Resource)]
pub struct Client<In, Out>
where
    In: std::io::Read,
    Out: std::io::Write,
{
    input: Deserializer<ReadReader<In>>,
    output: Out,
    id: u32,
}

#[derive(Debug)]
pub enum Error {
    Encode(encode::Error),
    Decode(decode::Error),
    InvalidType(MessageType, MessageType),
    InvalidId(u32, u32),
    Remote(String),
}

#[derive(Debug, PartialEq, Eq)]
pub enum MessageType {
    Request,
    Response,
    Notification,
}
struct Message<T> {
    tp: MessageType,
    id: u32,
    str: Option<String>,
    value: T,
}
struct PMessage<D> {
    data: D,
}

impl<'de, D> Visitor<'de> for PMessage<D>
where
    D: DeserializeSeed<'de>,
{
    type Value = Message<D::Value>;

    fn expecting(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "a msgpack rpc message")
    }

    fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
    where
        A: SeqAccess<'de>,
    {
        let tpcode = seq
            .next_element::<u64>()?
            .ok_or(de::Error::invalid_length(0, &"4"))?;
        let tp = match tpcode {
            0 => MessageType::Request,
            1 => MessageType::Response,
            2 => MessageType::Notification,
            _ => {
                return Err(de::Error::invalid_value(
                    de::Unexpected::Unsigned(tpcode),
                    &"0, 1 or 2",
                ))
            }
        };
        let id = seq
            .next_element::<u32>()?
            .ok_or(de::Error::invalid_length(1, &"4"))?;
        let error: Option<String> = seq
            .next_element()?
            .ok_or(de::Error::invalid_length(2, &"4"))?;
        let result = seq
            .next_element_seed(self.data)?
            .ok_or(de::Error::invalid_length(3, &"4"))?;
        if !seq.next_element::<()>()?.is_none() {
            return Err(de::Error::invalid_length(5, &"4"));
        }
        Ok(Message {
            tp,
            id,
            str: error,
            value: result,
        })
    }
}

impl<'de, D> DeserializeSeed<'de> for PMessage<D>
where
    D: DeserializeSeed<'de>,
{
    type Value = Message<D::Value>;

    fn deserialize<Dsr>(self, d: Dsr) -> Result<Self::Value, Dsr::Error>
    where
        Dsr: serde::Deserializer<'de>,
    {
        d.deserialize_seq(self)
    }
}

impl<T> Serialize for Message<T>
where
    T: Serialize,
{
    fn serialize<S>(&self, s: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let tpcode = match self.tp {
            MessageType::Request => 0,
            MessageType::Response => 1,
            MessageType::Notification => 2,
        };
        let mut tuple = s.serialize_tuple(4)?;
        tuple.serialize_element(&tpcode)?;
        tuple.serialize_element(&self.id)?;
        tuple.serialize_element(&self.str)?;
        tuple.serialize_element(&self.value)?;
        tuple.end()
    }
}

fn send_io_error(err: std::io::Error) -> Error {
    Error::Encode(rmp_serde::encode::Error::InvalidValueWrite(
        rmp::encode::ValueWriteError::InvalidDataWrite(err),
    ))
}

impl<In, Out> Client<In, Out>
where
    In: std::io::Read,
    Out: std::io::Write,
{
    pub fn new(input: In, output: Out) -> Self {
        Self {
            input: Deserializer::new(input),
            output,
            id: 0,
        }
    }

    pub fn send_msg<T>(&mut self, method: &str, params: T) -> Result<u32, Error>
    where
        T: Serialize,
    {
        let msg = Message {
            tp: MessageType::Request,
            id: self.id,
            str: Some(method.to_string()),
            value: params,
        };
        self.id += 1;
        let msgpack = rmp_serde::encode::to_vec(&msg).map_err(|err| Error::Encode(err))?;
        let mut file =
            std::fs::File::create(format!("request_{}.mp", self.id - 1)).map_err(send_io_error)?;
        file.write_all(&msgpack).map_err(send_io_error)?;
        self.output.write_all(&msgpack).map_err(send_io_error)?;
        self.output.flush().map_err(send_io_error)?;
        Ok(msg.id)
    }

    pub fn receive_msg<'de, T>(&mut self, id: u32, data: T) -> Result<T::Value, Error>
    where
        T: DeserializeSeed<'de>,
    {
        let parser: PMessage<T> = PMessage { data };
        let msg = parser
            .deserialize(&mut self.input)
            .map_err(|err| Error::Decode(err))?;
        log::debug!("Received {:#?} message (id: {})", msg.tp, msg.id);
        if msg.tp != MessageType::Response {
            return Err(Error::InvalidType(MessageType::Response, msg.tp));
        }
        if msg.id != id {
            return Err(Error::InvalidId(id, msg.id));
        }
        match msg.str {
            Some(err) => Err(Error::Remote(err)),
            None => Ok(msg.value),
        }
    }
}
