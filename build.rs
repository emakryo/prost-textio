use prost_reflect_build::Builder;

fn main() -> std::io::Result<()> {
    let file_descriptor_set_path = std::path::PathBuf::from(std::env::var("OUT_DIR").unwrap())
        .join("file_descriptor_set_test.bin");

    Builder::new()
        .file_descriptor_set_path(&file_descriptor_set_path)
        .compile_protos(
            &[
                "tests/proto/google/protobuf/unittest.proto",
                "tests/proto/google/protobuf/unittest_import.proto",
                "tests/proto/google/protobuf/unittest_proto3.proto",
                "tests/proto/google/protobuf/unittest_mset.proto",
                "tests/proto/google/protobuf/unittest_mset_wire_format.proto",
            ],
            &["tests/proto"],
        )
}
