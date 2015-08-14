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
```SELECT <attrs>
 FROM <tables>
 WHERE <condition>
 ORDER BY <order‐expr>```
<br><br>

#### Usage Examples
Single table
> SELECT &lt;attrs&gt; FROM &lt;table&gt;

Multiple tables
> SELECT &lt;attrs&gt; FROM [&lt;table1&gt; &lt;name1&gt;] [&lt;table2&gt; &lt;name2&gt;] ...

Filtering
> SELECT * FROM Person WHERE (equal? "Name" "David")

Order-by
> SELECT ("Name" "LikesChocolate") FROM Person ORDER BY "Age"
