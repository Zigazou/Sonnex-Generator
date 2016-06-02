<?php
mb_internal_encoding("UTF-8");

require_once "sonnex.php";

while($word = trim(fgets(STDIN))) {
  print $word . ", " . sonnex_sonnex($word) . "\n";
}
