package cz.upol.jj1;

public class Main {
    // Countries
    static Country cz = new Country("Česká republika", "+420");
    static Country sk = new Country("Slovensko", "+421");
    static Country[] countries = {cz, sk};

    // Persons (people)
    static Person one = new Person(0, "Prokop", "Dveře", 15, "555444333", cz);
    static Person two = new Person(1, "Tomáš", "Putna", 23, "999888777", cz);
    static Person three = new Person(2, "Fero", "Dunaj", 25, "123456789", sk);
    static Person four = new Person(3, "Johnny", "Cash", 71, "987654321", null);
    static Person five = new Person(4, "Tomáš", "Putna", 31, "111222333", null);
    static Person[] persons = {one, two, three, four, five};

    // Index
    static Index index = new Index(persons, countries);

    public static void main(String[] args) {
        // Ukol #1
        cz.print();
        sk.print();

        // Ukol #2, 3, 4
        one.print();
        three.print();
        four.print();

        // Ukol 5
        String countryName = "Česká republika";
        System.out.println("There are " + index.count(countryName) + " people living in " + countryName);

        // Ukol 6
        String firstName = "Tomáš";
        String lastName = "Putna";
        System.out.println("There are "
                + index.count(firstName, lastName) 
                + " people named "
                + firstName + " " + lastName);

        // Ukol 7
        int id = 2;
        Person personFound = index.search(id);
        System.out.println("The person with id " + id + " is named " + personFound.getFullName());

        // Ukol 8
        Person[] peopleFoundByCountry = index.search(countryName);
        for (Person person : peopleFoundByCountry) {
            System.out.println(person.getFullName() + " was found in " + countryName);
        }

        // Ukol 9
        Person[] peopleFoundByName = index.search(firstName, lastName);
        for (Person person : peopleFoundByName) {
            System.out.println("There is a " + person.getFullName() + " with id " + person.id);
        }

        // Ukol 11
        index.print();
    }
}
