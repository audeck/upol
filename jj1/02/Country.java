package cz.upol.jj1;

class Country {
  String name;
  String phoneCode;

  Country(String name, String phoneCode) {
    this.name = name;
    this.phoneCode = phoneCode;
  }

  void print() {
    String output = "Stát{jméno=" + this.name + ", předvolba=" + this.phoneCode +"}";
    System.out.println(output);
  }
}
