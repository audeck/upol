package cz.upol.jj1;

/** A timetable block representing an event */
public interface TimetableEvent { // Should extend TimetableBlock, but no inheritance this seminar
  /** @return this event's place (room code) */
  String getPlace();
}
