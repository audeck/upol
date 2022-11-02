package cz.upol.jj1;

/**
 * All used animal species. Each species contains its male & female taxonomy name, as well as the
 * interjection of sound its members are perceived to make.
 */
public enum AnimalSpecies {
  DOG("pes", "fena", "haf-haf"),
  DUCK("kačer", "kačena", "ga-ga");

  /** The species' male name */
  private final String maleName;
  /** The species' female name */
  private final String femaleName;
  /** The species' sound */
  private final String animalSound;

  AnimalSpecies(String maleName, String femaleName, String animalSound) {
    this.maleName = maleName;
    this.femaleName = femaleName;
    this.animalSound = animalSound;
  }

  /**
   * Returns this species' name based on a given gender.
   *
   * @param gender a given gender
   * @return this species' {@code maleName} if {@code gender} is {@code Gender.MALE}, otherwise
   *     {@code femaleName}
   */
  public String getName(Gender gender) {
    return (gender == Gender.MALE) ? this.maleName : this.femaleName;
  }

  /** @return this species' sound */
  public String getAnimalSound() {
    return this.animalSound;
  }
}
