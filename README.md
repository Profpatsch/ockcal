# µCal - the simplest possible calendar

## Usage

	µcal # lists the todos
	µcal add 2015-12-12 13:37:00 "Celebrate leettime" # adds the event "Celebrate leettime" on 2015-12-12 at 13:37:00

## F. A. Q.
### Why is the calendar file gets copied to a new location before writing?
This is because of Haskell's [lazy](https://en.wikipedia.org/wiki/Lazy_evaluation) IO. Haskell reads chunks of a file in on demand. This consumes less memory and spreads reading and calculating more or less homogenous across runtime.

This is actually also a feature: If a runtime error occurs whilst calculating the data to write, we still have a safe copy which in that case also would not be deleted.

## Credits
Inspired by Steve Losch's [t](https://github.com/sjl/t/).
