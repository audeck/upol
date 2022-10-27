package cz.upol.jj1;

/**
 * A simple class for animals.
 * @see cz.upol.jj1.AnimalFarm
 */
public class Animal {

  private String name;
  private AnimalSpecies species;
  private Gender gender;

  public Animal(String name, AnimalSpecies species, Gender gender) {
    this.name = name;
    this.species = species;
    this.gender = gender;
  }

  /**
   * @return The animal's name.
   */
  public String getName() {
    return this.name;
  }

  /**
   * @param name The animal's name.
   */
  public void setName(String name) {
    this.name = name;
  }

  public AnimalSpecies getSpecies() {
    return this.species;
  }

  public void setSpecies(AnimalSpecies species) {
    this.species = species;
  }

  public Gender getGender() {
    return this.gender;
  }

  public void setGender(Gender gender) {
    this.gender = gender;
  }

  public String getSpeciesName() {
    return this.species.getName(this.gender);
  }

  public String getSound() {
    return this.species.getAnimalSound();
  }
}
