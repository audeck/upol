package cz.upol.jj1;

/** A bare bones timetable block. */
public interface TimetableBlock {

  /** @return This block's title. */
  String getTitle();

  /** @return this block's type (subtitle) */
  String getType();

  /**
   * Returns this block's time. Can either be a single time (e.g. 8:00) or a time range
   * (e.g. 8:00-9:30).
   *
   * @return this block's time (range)
   */
  String getTime();

  /** @return this block's color */
  String getColor();  // Should be done with enums (again)
}
