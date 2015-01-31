# ockcal - the simplest possible calendar

## Usage

	ockcal # lists the todos
	ockcal add 2015-12-12 13:37:00 "Celebrate leettime" # adds the event "Celebrate leettime" on 2015-12-12 at 13:37:00

## F. A. Q.
### How do I sync my Calendar?
Use your favourite "Cloud" service, git-annex, Seafile, Owncloud, BTSync, syncthing etc. You could also just use good old `git`.

### How do I view my calendar if ockcal is not available?
Just open the file with a text editor!

![](http://pds.exblog.jp/imgc/i=http%253A%252F%252Fpds.exblog.jp%252Fpds%252F1%252F201002%252F12%252F90%252Fa0126590_22301391.jpg,small=800,quality=75,type=jpg)

	wow
	    such plain text
	 many editor
	   very simple
	 amaze!

### Why is the calendar file gets copied to a new location before writing?
This is because of Haskell's [lazy](https://en.wikipedia.org/wiki/Lazy_evaluation) IO. Haskell reads chunks of a file in on demand. This consumes less memory and spreads reading and calculating more or less homogenous across runtime.

This is actually also a feature: If a runtime error occurs whilst calculating the data to write, we still have a safe copy which in that case also would not be deleted.

### Why has it this stupid name?
Because of [Ockham's razor](https://en.wikipedia.org/wiki/Occam%27s_razor).

## Credits
Inspired by Steve Losch's [t](https://github.com/sjl/t/).

## License
[GPL](./LICENSE)
