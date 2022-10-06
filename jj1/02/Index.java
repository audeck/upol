package cz.upol.jj1;

import java.util.*;

public class Index {
    Person[] persons;
    Country[] countries;

    Index(Person[] persons, Country[] countries) {
        this.persons = persons;
        this.countries = countries;
    }

    /* Returns the number of people in living in "countryName" */
    public int count(String countryName) {
        int count = 0;

        for (Person person : this.persons) {
            if (person.country != null && person.country.name.equals(countryName))
                count += 1;
        }

        return count;
    }

    /* Returns the number of people named "firstName lastName" */
    public int count(String firstName, String lastName) {
        int count = 0;

        for (Person person : this.persons) {
            if (person.firstName.equals(firstName) && person.lastName.equals(lastName))
                count += 1;
        }

        return count;
    }

    /* Returns the person with a given id (returns null if not found) */
    public Person search(int id) {
        for (Person person : this.persons) {
            if (person.id == id) return person;
        }
        return null;
    }

    /* Returns an array of all people living in "countryName" */
    public Person[] search(String countryName) {
        Person[] output = new Person[count(countryName)];
        int outputIndex = 0;

        for (Person person : this.persons) {
            if (person.country != null && person.country.name.equals(countryName)) {
                output[outputIndex] = person;
                outputIndex += 1;
            }
        }

        return output;
    }

    /* Returns an array of all people named "firstName lastName" */
    public Person[] search(String firstName, String lastName) {
        Person[] output = new Person[count(firstName, lastName)];
        int outputIndex = 0;

        for (Person person : this.persons) {
            if (person.firstName.equals(firstName) && person.lastName.equals(lastName)) {
                output[outputIndex] = person;
                outputIndex += 1;
            }
        }

        return output;
    }

    /* Sets a person's country to a country given by phone code */
    public boolean setCountryByPhoneCode(Person person, String phoneCode) {
        for (Country country : this.countries) {
            if (country.phoneCode.equals(phoneCode)) {
                person.country = country;
                return true;
            }
        }

        return false;
    }

    /* Prints out the default index string */
    public void print() {
        StringBuilder output = new StringBuilder("Rejstřík{ ");

        for (Person person : this.persons) {
            output.append(person.getIndexString());
            output.append(" ");
        }

        output.append(" }");
        System.out.println(output.toString());
    }
}
