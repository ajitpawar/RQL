### Racket Query Language (RQL)

An RDBMS database written in Racket, with SQL-like querying &lt;br&gt;
Syntax for RQL is similar to SQL

#### Commands supported
* SELECT
* FROM
* WHERE
* ORDER BY
* GROUP BY
<br><br>

#### General Syntax
```
SELECT <attrs>
 FROM <tables>
 WHERE <condition>
 ORDER BY <order‐expr> 
```

#### Usage Examples

Single table <br>
```SELECT <attrs> FROM <table>```

Multiple tables <br>
```SELECT <attrs> FROM [<table1> <name1>] [<table2> <name2>] ... ```

Filtering <br>
```SELECT * FROM  Person WHERE (equal? "Name" "David")```

Order-by <br>
```SELECT ("Name" "LikesChocolate") FROM Person ORDER BY "Age"```

