#![allow(unused)]
#[derive(PartialEq, Eq, Debug, Clone)]
pub enum DataType {
    UInt8,
    UInt16,
    UInt32,
    UInt64,
    Int8,
    Int16,
    Int32,
    Int64,
    Float64,
    Float32,
    Pointer,
}

impl DataType {
    pub fn from_str(str: String) -> Option<DataType> {
        match str.as_str() {
            "uint8" => Some(DataType::UInt8),
            "uint16" => Some(DataType::UInt16),
            "uint32" => Some(DataType::UInt32),
            "uint64" => Some(DataType::UInt64),
            "int8" => Some(DataType::Int8),
            "int16" => Some(DataType::Int16),
            "int32" => Some(DataType::Int32),
            "int64" => Some(DataType::Int64),
            "float64" => Some(DataType::Float64),
            "float32" => Some(DataType::Float32),
            "ptr" => Some(DataType::Pointer),
            _ => None,
        }
    }
}
