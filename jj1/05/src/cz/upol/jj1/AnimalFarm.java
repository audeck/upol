package cz.upol.jj1;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * A class which implements a simple animal farm, which contains a list of animals and basic
 * functionality.
 */
public class AnimalFarm {
  /** The farm's animals */
  private List<Animal> animals;

  public AnimalFarm(Animal... animals) {
    this.animals = new ArrayList<Animal>();
    this.animals.addAll(Arrays.asList(animals));
  }

  /**
   * Adds a given animal to this animal farm.
   *
   * @param animal a given animal
   */
  public void add(Animal animal) {
    this.animals.add(animal);
  }

  /** Prints out a descriptive line of text for each animal in this animal farm. */
  public String list() {
    StringBuilder output = new StringBuilder();

    for (Animal animal : this.animals) {
      output.append(animal.getName())
          .append(" je ")
          .append(animal.getSpeciesName())
          .append(" a dělá ")
          .append(animal.getSound())
          .append(".\n");
    }

    System.out.println(output);
    return output.toString();
  }
}
