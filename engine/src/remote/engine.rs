pub trait TermEngine {
    type R: super::Remote;

    fn remote<'a>(&'a mut self) -> &'a mut Self::R;
    fn is_funct_obj(&mut self, obj: u64, cat: u64) -> Option<(u64, u64, u64)>;
    fn is_identity(&mut self, obj: u64, cat: u64) -> Option<u64>;
    fn is_comp(&mut self, obj: u64, cat: u64) -> Option<(u64, u64, u64, u64, u64)>;
    fn is_funct_mph(&mut self, obj: u64, cat: u64) -> Option<(u64, u64, u64, u64, u64)>;
}
