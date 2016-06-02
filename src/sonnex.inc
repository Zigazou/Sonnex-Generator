<?php

function _sonnex_is_vowel($char) {
  static $VOWELS = array(
  'a', 'â', 'à', 'ä', 'e', 'é', 'è', 'ê', 'ë', 'i', 'î', 'ï', 'o', 'ô',
  'ö', 'u', 'ù', 'û', 'ü', 'y', 'œ'
  );

  return in_array($char, $VOWELS);
}

function _sonnex_is_conson($char) {
  static $CONSONS = array(
  'b', 'c', 'ç', 'd', 'f', 'g', 'h', 'j', 'k', 'l', 'm', 'n', 'p', 'q',
  'r', 's', 't', 'v', 'w', 'x', 'y', 'z'
  );

  return in_array($char, $CONSONS);
}

function _sonnex_sonx($st) {

  if($st == "") return "";

  $length = mb_strlen($st);
  $head = mb_substr($st, 0, 1);
  $tail = mb_substr($st, 1);
  $st2 = "";
  $st3 = "";
  $st4 = "";
  $st5 = "";

  if ($length >= 5) {
    $st5 = mb_substr($st, 0, 5);
  }
  if ($length >= 4) {
    $st4 = mb_substr($st, 0, 4);
  }
  if ($length >= 3) {
    $st3 = mb_substr($st, 0, 3);
  }
  if ($length >= 2) {
    $st2 = mb_substr($st, 0, 2);
  }

  // Starting with 'e'
  if($head == "e") {
    if($st == "e") return "";
    if($st == "ec") return "Ec";
    if($st == "ef") return "Ef";
    if($st == "eaux") return "o";
    if($st4 == "eann") return "an"._sonnex_sonx(mb_substr($st, 4));
    if($st3 == "ean") return '2'._sonnex_sonx(mb_substr($st, 3));
    if($st3 == "eau") return 'o'._sonnex_sonx(mb_substr($st, 3));
    if($st3 == "eff") return "Ef"._sonnex_sonx(mb_substr($st, 3));
    if($st3 == "egm") return 'E'._sonnex_sonx("gm".mb_substr($st, 3));
    if($st == "ein") return "1";
    if($st3 == "ein") {
      $c = mb_substr($st, 3, 1);
      $cs = mb_substr($st, 4);
      if($c == 'n') return "En"._sonnex_sonx($cs);
      if(_sonnex_is_vowel($c)) return "En"._sonnex_sonx($c.$cs);
      return '1'._sonnex_sonx($c.$cs);
    }
    if($st2 == "ei") return 'E'._sonnex_sonx(mb_substr($st, 2));
    if($st3 == "ell") return "El"._sonnex_sonx(mb_substr($st,3));
    if($st2 == "el") {
      $c = mb_substr($st, 2, 1);
      $cs = mb_substr($st, 3);
      if(_sonnex_is_conson($c)) return 'E'._sonnex_sonx('l'.$c.$cs);
      return 'e'._sonnex_sonx('l'.$c.$cs);
    }
    if($st3 == "emm") return "Em"._sonnex_sonx(mb_substr($st,3));
    if($st3 == "emp") return '2'._sonnex_sonx(mb_substr($st,3));
    if($st3 == "enn") return "En"._sonnex_sonx(mb_substr($st,3));
    if($st == "en") return "2";
    if($st2 == "en") {
      $c = mb_substr($st, 2, 1);
      $cs = mb_substr($st, 3);
      if(_sonnex_is_vowel($c)) return "en"._sonnex_sonx($c.$cs);
      return '2'._sonnex_sonx($c.$cs);
    }
    if($st == "er") return "E";
    if($st == "ert") return "Er";
    if($st3 == "err") return "Er"._sonnex_sonx(mb_substr($st,3));
    if($st3 == "erf") return "Er"._sonnex_sonx('f'.mb_substr($st,3));
    if($st == "es") return "";
    if($st4 == "esch") return "EC"._sonnex_sonx(mb_substr($st,4));
    if($st == "essent") return "Es";
    if($st2 == "es") {
      $c = mb_substr($st, 2, 1);
      $cs = mb_substr($st, 3);
      if($c == 'h') return 'E'._sonnex_sonx('h'.$cs);
      if($c == 'n') return 'E'._sonnex_sonx('n'.$cs);
      if($c == 's') return "Es"._sonnex_sonx($cs);
      if(_sonnex_is_conson($c)) return "Es"._sonnex_sonx($c.$cs);
      return "ez"._sonnex_sonx($c.$cs);
    }
    if($st2 == "és") {
      $c = mb_substr($st, 2, 1);
      $cs = mb_substr($st, 3);
      if($c == 's') return "Es"._sonnex_sonx($cs);
      if(_sonnex_is_conson($c)) return "Es"._sonnex_sonx($c.$cs);
      return "Ez"._sonnex_sonx($c.$cs);
    }
    if($st3 == "ett") return "Et"._sonnex_sonx(mb_substr($st, 4));
    if($st == "et") return "E";
    if($st2 == "et") return "et"._sonnex_sonx(mb_substr($st, 2));
    if($st3 == "eun") {
      $c = mb_substr($st, 3, 1);
      $cs = mb_substr($st, 4);
      if(_sonnex_is_vowel($c)) return "en"._sonnex_sonx($c.$cs);
      return '1'._sonnex_sonx($c.$cs);
    }
    if($st == "eux") return "e";
    if($st4 == "euxi") return "ez"._sonnex_sonx('i'.mb_substr($st, 4));
    if($st2 == "eu") return 'e'._sonnex_sonx(mb_substr($st, 2));
    if($st == "ex") return "Eks";
    if($st2 == "ey") {
      $c = mb_substr($st, 2, 1);
      $cs = mb_substr($st, 3);
      if(_sonnex_is_conson($c)) return 'E'._sonnex_sonx($c.$cs);
      return 'E'._sonnex_sonx('y'.$c.$cs);
    }
    if($st == "ez") return "E";
    return $head._sonnex_sonx($tail);
  }

  // Starting with 's'
  if($head == "s") {
    if($st == "s") return "";
    if($st2 == "ss") return 's'._sonnex_sonx(mb_substr($st, 2));
    if($st2 == "st") return "st"._sonnex_sonx(mb_substr($st, 2));
    if($st3 == "sci") return 's'._sonnex_sonx('i'.mb_substr($st, 3));
    return $head._sonnex_sonx($tail);
  }

  // Starting with 'a'
  if($head == "a") {
    if($st == "a") return "a";
    if($st == "aient") return "E";
    if($st == "ain") return "1";
    if($st3 == "ain") {
      $v = mb_substr($st, 3, 1);
      $cs = mb_substr($st, 4);
      if(_sonnex_is_vowel($v)) return "E"._sonnex_sonx("n".$v.$cs);
      return "1"._sonnex_sonx($v.$cs);
    }
    if($st == "ais") return "E";
    if($st3 == "ais") {
      $v = mb_substr($st, 3, 1);
      $cs = mb_substr($st, 4);
      if($v == 's') return "Es"._sonnex_sonx($cs);
      if(_sonnex_is_vowel($v)) return "Ez"._sonnex_sonx($v.$cs);
      return "Es"._sonnex_sonx($v.$cs);
    }
    if($st == "ail") return "ai";
    if($st4 == "aill") return "ai"._sonnex_sonx(mb_substr($st, 4));
    if($st2 == "ai") return 'E'._sonnex_sonx(mb_substr($st, 2));
    if($st3 == "amm") return "am"._sonnex_sonx(mb_substr($st, 3));
    if($st2 == "am") {
      $c = mb_substr($st, 2, 1);
      $cs = mb_substr($st, 3);
      if($c == 'm') return "am"._sonnex_sonx($cs);
      if(_sonnex_is_vowel($c)) return "am"._sonnex_sonx($c.$cs);
      return '2'._sonnex_sonx($c.$cs);
    }
    if($st == "an") return "2";
    if($st2 == "an") {
      $c = mb_substr($st, 2, 1);
      $cs = mb_substr($st, 3);
      if($c == 'n') return "an"._sonnex_sonx($cs);
      if($c == 't' and $cs=="") return "2";
      if($c == 'te') return "2t"._sonnex_sonx($cs);
      if(_sonnex_is_vowel($c)) return "an"._sonnex_sonx($c.$cs);
      return '2'._sonnex_sonx($c.$cs);
      return $head._sonnex_sonx($tail);
    }
    if($st == "assent") return "as";
    if($st2 == "as") {
      $c = mb_substr($st, 2, 1);
      $cs = mb_substr($st, 3);
      if($c == 's') return "as"._sonnex_sonx($cs);
      if(_sonnex_is_conson($c)) return "as"._sonnex_sonx($c.$cs);
      return "az"._sonnex_sonx($c.$cs);
    }
    if($st2 == "au") return 'o'._sonnex_sonx(mb_substr($st, 2));
    if($st == "ay") return "E";
    if($st == "ays") return "E";
    return $head._sonnex_sonx($tail);
  }

  // Starting with 'i'
  if($head == "i") {
    if($st == "ic") return "ik";
    if($st == "ics") return "ik";
    if($st4 == "ienn") return "iEn"._sonnex_sonx(mb_substr($st, 4));
    if($st3 == "ien") return "i1"._sonnex_sonx(mb_substr($st, 3));
    if($st == "in") return "1";
    if($st2 == "in") {
      $c = mb_substr($st, 2, 1);
      $cs = mb_substr($st, 3);
      if($c == 'n') return "in"._sonnex_sonx($cs);
      if(_sonnex_is_vowel($c)) return "in"._sonnex_sonx($c.$cs);
      return '1'._sonnex_sonx($c.$cs);
    }
    if($st == "issent") return "is";
    if($st == "is") return "i";
    if($st2 == "is") {
      $c = mb_substr($st, 2, 1);
      $cs = mb_substr($st, 3);
      if($c == 's') return "is"._sonnex_sonx($cs);
      if(_sonnex_is_conson($c)) return "is"._sonnex_sonx($c.$cs);
      return "iz"._sonnex_sonx($c.$cs);
    }
    if($st3 == "ixi") return "iz"._sonnex_sonx('i'.mb_substr($st, 3));
    if($st3 == "ill") return 'i'._sonnex_sonx(mb_substr($st, 3));
    if($head == "i") return 'i'._sonnex_sonx($tail);
    return $head._sonnex_sonx($tail);
  }

  // Starting with 't'
  if($head == "t") {
    if($st == "t") return "";
    if($st == "ts") return "";
    if($st4 == "tier") return 't'._sonnex_sonx("ier".mb_substr($st,4));
    if($st2 == "ti") {
      $v = mb_substr($st, 2, 1);
      $cs = mb_substr($st, 3);
      if(_sonnex_is_vowel($v)) return 's'._sonnex_sonx('i'.$v.$cs);
      return 't'._sonnex_sonx('i'.$v.$cs);
    }
    if($st2 == "tt") return 't'._sonnex_sonx(mb_substr($st, 2));
    return $head._sonnex_sonx($tail);
  }

  // Starting with 'n'
  if($head == "n") {
    if($st2 == "nn") return 'n'._sonnex_sonx(mb_substr($st, 2));
    return $head._sonnex_sonx($tail);
  }

  // Starting with 'r'
  if($head == "r") {
    if($st2 == "rr") return 'r'._sonnex_sonx(mb_substr($st, 2));
    return $head._sonnex_sonx($tail);
  }

  // Starting with 'u'
  if($head == "u") {
    if($st == "un") return "1";

    if($st == "ussent") return "us";
    if($st2 == "us") {
      $c = mb_substr($st, 2, 1);
      $cs = mb_substr($st, 3);
      if($c == 's') return "us"._sonnex_sonx($cs);
      if(_sonnex_is_conson($c)) return "us"._sonnex_sonx($c.$cs);
      return "uz"._sonnex_sonx($c.$cs);
    }
    return $head._sonnex_sonx($tail);
  }

  // Starting with 'l'
  if($head == "l") {
    if($st2 == "ll") return 'l'._sonnex_sonx(mb_substr($st, 2));
    return $head._sonnex_sonx($tail);
  }

  // Starting with 'o'
  if($head == "o") {
    if($st3 == "occ") return "ok"._sonnex_sonx(mb_substr($st, 3));
    if($st3 == "oeu") return 'e'._sonnex_sonx(mb_substr($st, 3));
    if($st2 == "œu") return 'e'._sonnex_sonx(mb_substr($st, 2));
    if($st == "oient") return "Ua";
    if($st3 == "oin") return "U1"._sonnex_sonx(mb_substr($st, 3));
    if($st3 == "ois") return "Ua"._sonnex_sonx(mb_substr($st, 3));
    if($st3 == "omm") return "om"._sonnex_sonx(mb_substr($st, 3));
    if($st2 == "om") {
      $c = mb_substr($st, 2, 1);
      $cs = mb_substr($st, 3);
      if(_sonnex_is_vowel($c)) return "om"._sonnex_sonx($c.$cs);
      return '3'._sonnex_sonx($c.$cs);
    }
    if($st3 == "onn") return "on"._sonnex_sonx(mb_substr($st, 3));
    if($st2 == "on") {
      $c = mb_substr($st, 2, 1);
      $cs = mb_substr($st, 3);
      if(_sonnex_is_vowel($c)) return "on"._sonnex_sonx($c.$cs);
      return "3"._sonnex_sonx($c.$cs);
    }
    if($st == "ossent") return "os";
    if($st2 == "os") {
      $c = mb_substr($st, 2, 1);
      $cs = mb_substr($st, 3);
      if($c == 's') return "os"._sonnex_sonx($cs);
      if(_sonnex_is_conson($c)) return "os"._sonnex_sonx($c.$cs);
      return "oz"._sonnex_sonx($c.$cs);
    }
    if($st2 == "ou") return 'U'._sonnex_sonx(mb_substr($st, 2));
    if($st2 == "où") return 'U'._sonnex_sonx(mb_substr($st, 2));
    if($st2 == "oû") return 'U'._sonnex_sonx(mb_substr($st, 2));
    return $head._sonnex_sonx($tail);
  }

  // Starting with 'd'
  if($head == "d") {
    if($st == "d") return "";
    if($st == "ds") return "";
    if($st2 == "dd") return 'd'._sonnex_sonx(mb_substr($st, 2));
    return $head._sonnex_sonx($tail);
  }

  // Starting with 'c'
  if($head == "c") {
    if($st == "c") return "";
    if($st2 == "ca") return 'k'._sonnex_sonx('a'.mb_substr($st, 2));
    if($st2 == "cc") {
      $v = mb_substr($st, 2, 1);
      $cs = mb_substr($st, 3);
      if($v == 'o') return 'k'._sonnex_sonx('o'.$cs);
      if($v == 'u') return 'k'._sonnex_sonx('u'.$cs);
      return "ks"._sonnex_sonx($cs);
    }
    if($st2 == "ce") return 's'._sonnex_sonx('e'.mb_substr($st, 2));
    if($st2 == "c'") return 's'._sonnex_sonx(mb_substr($st, 2));
    if($st2 == "c’") return 's'._sonnex_sonx(mb_substr($st, 2));
    if($st4 == "chao") return 'k'._sonnex_sonx("ao".mb_substr($st, 4));
    if($st3 == "chl") return "kl"._sonnex_sonx(mb_substr($st, 3));
    if($st4 == "choe") return 'k'._sonnex_sonx("oe".mb_substr($st, 4));
    if($st3 == "chœ") return 'k'._sonnex_sonx("oe".mb_substr($st, 3));
    if($st3 == "chr") return "kr"._sonnex_sonx(mb_substr($st, 3));
    if($st2 == "ch") return 'C'._sonnex_sonx(mb_substr($st, 2));
    if($st2 == "ci") return 's'._sonnex_sonx('i'.mb_substr($st, 2));
    if($st2 == "ck") return 'k'._sonnex_sonx(mb_substr($st, 2));
    if($st4 == "coeu") return 'k'._sonnex_sonx ("oeu".mb_substr($st, 4));
    if($st5 == "compt") return "k3t"._sonnex_sonx(mb_substr($st, 5));
    if($st3 == "cœu") return 'k'._sonnex_sonx("œu".mb_substr($st, 3));
    if($st2 == "co") return 'k'._sonnex_sonx('o'.mb_substr($st, 2));
    if($st4 == "cuei") return "ke"._sonnex_sonx('i'.mb_substr($st, 4));
    if($st2 == "cu") return 'k'._sonnex_sonx('u'.mb_substr($st, 2));
    if($st2 == "cy") return 's'._sonnex_sonx('y'.mb_substr($st, 2));
    if($head == "c") return 'k'._sonnex_sonx($tail);
    if($head == 'ç') return 's'._sonnex_sonx($tail);
    return $head._sonnex_sonx($tail);
  }

  // Starting with 'p'
  if($head == "p") {
    if($st == "p") return "";
    if($st == "ps") return "";
    if($st2 == "ph") return 'f'._sonnex_sonx(mb_substr($st, 2));
    if($st2 == "pp") return 'p'._sonnex_sonx(mb_substr($st, 2));
    if($st4 == "pays") return "pE"._sonnex_sonx ("is".mb_substr($st, 4));
    return $head._sonnex_sonx($tail);
  }

  // Starting with 'm'
  if($head == "m") {
    if($st2 == "mm") return 'm'._sonnex_sonx(mb_substr($st, 2));
    return $head._sonnex_sonx($tail);
  }

  //Starting with 'é'
  if($head == 'é') {
    if($st == "é") return "E";
    $c = mb_substr($st, 1, 1);
    $cs = mb_substr($st, 2);
    if($c == 't')  return "Et"._sonnex_sonx($cs);
    return 'E'._sonnex_sonx($c.$cs);
  }

  // Starting with 'v'
  if($head == 'v') return 'v'._sonnex_sonx($tail);

  // Starting with 'q'
  if($head == "q") {
    if($st3 == "qur") return "ku"._sonnex_sonx('r'.mb_substr($st, 3));
    if($st2 == "qu") return 'k'._sonnex_sonx(mb_substr($st, 2));
    if($head == 'q') return 'k'._sonnex_sonx($tail);
    return $head._sonnex_sonx($tail);
  }

  // Starting with 'f'
  if($head == "f") {
    if($st2 == "ff") return 'f'._sonnex_sonx(mb_substr($st, 2));
    return $head._sonnex_sonx($tail);
  }

  // Starting with 'b'
  if($head == "b") {
    if($st == "b") return "";
    if($st2 == "bb") return 'b'._sonnex_sonx(mb_substr($st, 2));
    return $head._sonnex_sonx($tail);
  }

  // Starting with 'g'
  if($head == "g") {
    if($st == "g") return "";
    if($st == "ge") return 'j';
    if($st2 == "ge") return 'j'._sonnex_sonx(mb_substr($st, 2));
    if($st2 == "gé") return 'jE'._sonnex_sonx(mb_substr($st, 2));
    if($st2 == "gi") return 'ji'._sonnex_sonx(mb_substr($st, 2));
    if($st2 == "gn") return 'n'._sonnex_sonx(mb_substr($st, 2));
    if($st2 == "gy") return 'j'._sonnex_sonx('y'.mb_substr($st, 2));
    if($st3 == "guë") return "gu"._sonnex_sonx(mb_substr($st, 2));
    if($st2 == "gu") return 'g'._sonnex_sonx(mb_substr($st, 2));
    if($st2 == "gg") return 'g'._sonnex_sonx(mb_substr($st, 2));
    return $head._sonnex_sonx($tail);
  }

  // Starting with 'h'
  if($head == "h") {
    return _sonnex_sonx($tail);
  }

  if($head == 'j') return 'j'._sonnex_sonx($tail);

  //starting with 'à'
  if($head == 'à') return 'a'._sonnex_sonx($tail);

  // Starting with 'x'
  if($head == "x") {
    if($st == "x") return "";
    if($head == 'x') {
      $c = mb_substr($st, 2, 1);
      $cs = mb_substr($st, 3);
      if($c == 'c') return "ks"._sonnex_sonx($cs);
      if(_sonnex_is_vowel($c)) return "kz"._sonnex_sonx($c.$cs);
      return "ks"._sonnex_sonx($c.$cs);
    }
    return $head._sonnex_sonx($tail);
  }

  // Starting with 'y'
  if($head == "y") return 'i'._sonnex_sonx($tail);

   // Starting with 'è'
  if($head == 'è') return 'E'._sonnex_sonx($tail);

  // Starting with 'ê'
  if($head == 'ê') {
    $c = mb_substr($st, 1, 1);
    $cs = mb_substr($st, 2);
    if($c == 't') return 'E'._sonnex_sonx('t'.$cs);
    return 'E'._sonnex_sonx($c.$cs);
  }

  // Starting with 'z'
  if($head == "z") {
    if($st2 == "zz") return 'z'._sonnex_sonx(mb_substr($st, 2));
    return $head._sonnex_sonx($tail);
  }

  // Starting with 'w'
  if($head == "w") return 'v'._sonnex_sonx($tail);

  // Starting with 'ç'
  if($head == "ç") return 's'._sonnex_sonx($tail);

  // Starting with 'ù'
  if($head == "ù") return 'u'._sonnex_sonx($tail);

  // Starting with 'k'
  if($head == "k") return 'k'._sonnex_sonx($tail);

  // Starting with 'î'
  if($head == "î") return "i"._sonnex_sonx($tail);

  // Starting with 'œ'
  if($head == "œ") return 'e'._sonnex_sonx($tail);

  // Starting with 'ï'
  if($head == "ï") return 'i'._sonnex_sonx($tail);

  //Starting with 'ël'
  if($head == 'ël') {
    if($st2 == "ël") return 'E'._sonnex_sonx('l'.mb_substr($st, 2));
    return $head._sonnex_sonx($tail);
  }

  //Starting with 'ô'
  if($head == "ô") return 'o'._sonnex_sonx($tail);

  // Starting with 'ö'
  if($head == "ö") return 'o'._sonnex_sonx($tail);

  // Starting with 'û'
  if($head == "û") return 'u'._sonnex_sonx($tail);

  // Apostroph is ignored/silent
  if($head == "'") return _sonnex_sonx($tail);
  if($head == "’") return _sonnex_sonx($tail);

  //starting with 'â'
  if($head == 'â') {
    $c = mb_substr($st, 1, 1);
    $cs = mb_substr($st, 2);
    if($c == 't') return 'a'._sonnex_sonx('t'.$cs);
    return 'a'._sonnex_sonx($c.$cs);
  }

  // Unnown character
  return _sonnex_sonx($tail);
}

// Compute a Sonnex code for a french word.
//
// The string must contain only one word".
// Each character should be considered as being vocal, not silent
function sonnex_sonnex($st) {
  static $cache = array();

  if(isset($cache[$st])) {
    return $cache[$st];
  }

  $st = mb_strtolower($st);
  switch($st) {
    case "cerf": $st = "sEr"; break;
    case "cerfs": $st = "sEr"; break;
    case "de" : $st = "de"; break;
    case "est": $st = "E"; break;
    case "es" : $st = "E"; break;
    case "huit": $st = "uit"; break;
    case "les": $st = "lE"; break;
    case "mer": $st = "mEr"; break;
    case "mes": $st = "mE"; break;
    case "ressent": $st = "res2"; break;
    case "serf": $st = "sEr"; break;
    case "serfs": $st = "sEr"; break;
    case "sept": $st = "sEt"; break;
    case "septième": $st = "sEtiEm"; break;
    case "ses": $st = "sE"; break;
    case "tes": $st = "tE"; break;
    default:
      $st4 = "";

      if (mb_strlen($st) >= 4) {
        $st4 = mb_substr($st, 0, 4);
      }

      if($st4 == "tien") return 't'._sonnex_sonx("ien".mb_substr($st, 4));

      $cache =$st;

      return _sonnex_sonx($st);
  }
}
