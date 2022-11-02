package cz.upol.jj1;

/**
 * A simple class for animals for an animal farm.
 *
 * @see cz.upol.jj1.AnimalFarm
 */
public class Animal {
  /** The animal's name */
  private String name;
  /** The animal's species */
  private AnimalSpecies species;
  /** The animal's gender */
  private Gender gender;

  public Animal(String name, AnimalSpecies species, Gender gender) {
    this.name = name;
    this.species = species;
    this.gender = gender;
  }

  /**
   * Returns this animal's species name as string based on its gender.
   *
   * @return this animal's species name
   */
  public String getSpeciesName() {
    return this.species.getName(this.gender);
  }

  /**
   * Returns the sound this animal makes based on its species.
   *
   * @return this animal's sound
   */
  public String getSound() {
    return this.species.getAnimalSound();
  }

  // Getters & setters

  public String getName() {
    return this.name;
  }

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
}
