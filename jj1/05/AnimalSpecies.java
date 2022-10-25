package cz.upol.jj1;

public enum AnimalSpecies {
  DOG("pes", "fena", "haf-haf"),
  DUCK("kačer", "kačena", "ga-ga");

  private String maleName;
  private String femaleName;
  private String animalSound;

  AnimalSpecies(String maleName, String femaleName, String animalSound) {
    this.maleName = maleName;
    this.femaleName = femaleName;
    this.animalSound = animalSound;
  }

  public String getName(boolean isMale) {
    return (isMale) ? this.maleName : this.femaleName;
  }

  public String getAnimalSound() {
    return this.animalSound;
  }
}
