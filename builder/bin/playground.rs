use derive_builder::Builder;

#[derive(Builder,Debug)]
pub struct Command {
    executable: String,
    args: Vec<String>,
    #[builder(each = "e")]
    env: Vec<String>,
    current_dir: Option<String>,
}

fn main() {
    let mut builder = Command::builder();
    builder.executable("cargo".to_owned());
    builder.args(vec!["build".to_owned(), "--release".to_owned()]);
    builder.e("foo=1".to_owned());
    builder.e("bar=2".to_owned());
    println!(concat!("{:?}", "foo"), builder.build());
}
