# IPCMParser
**IPCM rules parser**

**Reads in rules in olain text and returns XML structure with automatic Id and parent Id assignment**

*For example*
~~~
Or Is A "String A"
   Includes B "String B"
   And Is C "String C"
       Includes D "String D"
~~~
*Becomes (Tags are not exact)*
~~~
<rule_id>1</rule_id>
<operator>OR</operator>

<rule_id>2</rule_id>
<parent_id>1</parent_id>
<operator>Is</operator>
<operand>A</operand>
<value>String A</value>

<rule_id>3</rule_id>
<parent_id>1</parent_id>
<operator>Includes</operator>
<operand>B</operand>
<value>String B</value>

<rule_id>4</rule_id>
<operator>And</operator>
<parent_id>1</parent_id>

<rule_id>5</rule_id>
<parent_id>4</parent_id>
<operator>Is</operator>
<operand>C</operand>
<value>String C</value>

<rule_id>6</rule_id>
<parent_id>4</parent_id>
<operator>Includes</operator>
<operand>D</operand>
<value>String D</value>
~~~

