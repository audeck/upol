package cz.upol.jj1;

public class Main {

  static Country czechia = new Country("Česká republika", "+420");
  static Country slovakia = new Country("Slovensko", "+421");
  static Country[] countries = {czechia, slovakia};

  static Person one = new Person(0, "Prokop", "Dveře", 15, "555444333", czechia);
  static Person two = new Person(1, "Tomáš", "Putna", 23, "999888777", czechia);
  static Person three = new Person(2, "Fero", "Dunaj", 25, "123456789", slovakia);
  static Person four = new Person(3, "Johnny", "Cash", 71, "987654321", null);
  static Person five = new Person(4, "Tomáš", "Putna", 31, "111222333", null);
  static Person[] persons = {one, two, three, four, five};

  static Index index = new Index(persons, countries);

  public static void main(String[] args) {
    czechia.print();
    slovakia.print();

    one.print();
    three.print();
    four.print();

    String countryName = "Česká republika";
    System.out.println(
        "There are " + index.count(countryName) + " people living in " + countryName);

    String firstName = "Tomáš";
    String lastName = "Putna";
    System.out.println(
        "There are "
            + index.count(firstName, lastName)
            + " people named "
            + firstName
            + " "
            + lastName);

    int id = 2;
    Person personFound = index.search(id);
    System.out.println("The person with id " + id + " is named " + personFound.getFullName());

    Person[] peopleFoundByCountry = index.search(countryName);
    for (Person person : peopleFoundByCountry) {
      System.out.println(person.getFullName() + " was found in " + countryName);
    }

    Person[] peopleFoundByName = index.search(firstName, lastName);
    for (Person person : peopleFoundByName) {
      System.out.println("Found a " + person.getFullName() + " with id " + person.id);
    }

    index.print();
  }
}
