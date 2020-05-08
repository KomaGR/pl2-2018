<?php
if ("${_SERVER['QUERY_STRING']}" == "") {
    $self = "${_SERVER['PHP_SELF']}";
} else {
    $self = "${_SERVER['PHP_SELF']}?${_SERVER['QUERY_STRING']}";
}

if (isset($_GET['cheat'])) {
    $cheat = true;
} else {
    $cheat = false;
}

if (isset($_GET['limit'])) {
    $limit = $_GET['limit'];
} else {
    $limit = 10;
}

if (isset($_GET['reset'])) {
    session_reset();
} else {
    session_start();
}

function prime($p)
{
    if ($p == 2) {
        return true;
    }

    if ($p % 2 == 0) {
        return false;
    }

    for ($i = 3; $i * $i <= $p; $i += 2) {
        if ($p % $i == 0) {
            return false;
        }
    }

    return true;
}

$permited_chars = "abcdefghijklmnopqrstuvwxyz";

function generate_string($chars = "abcdefghijklmnopqrstuvwxyz", $length = 6)
{
    $chars_length = strlen($chars);
    $output = "";
    for ($i = 0; $i < $length; $i++) {
        $randome_char = $chars[mt_rand(0, $chars_length - 1)];
        $output .= $randome_char;
    }

    return $output;
}

function microtime_float()
{
    list($usec, $sec) = explode(" ", microtime());
    return ((float) $usec + (float) $sec);
}

function get_int_max()
{
    $max = 0x7fff;
    $probe = 0x7fffffff;
    while ($max == ($probe >> 16)) {
        $max = $probe;
        $probe = ($probe << 16) + 0xffff;
    }
    return $max;
}

if (!defined('PHP_INT_MAX')) {
    define('PHP_INT_MAX', get_int_max());
}

$max = array();
$max[0] = 2;
$max[1] = 10;
$max[2] = 100;
$max[3] = 1000;
$max[4] = 10000;
$max[5] = 100000;
$max[6] = 1000000;
$max[7] = 10000000;
$max[8] = 100000000;
$max[9] = 1000000000;
$max[10] = PHP_INT_MAX;
if ($max[10] > 10000000000) {
    $max[10] = 10000000000;
}

function generate()
{
    global $max;
    global $permited_chars;

    $_SESSION['count']++;
    $num = rand($max[$_SESSION['count'] - 1],
        $max[$_SESSION['count']] - 1);

    $length = rand($max[$_SESSION['count'] - 1],
        $max[$_SESSION['count']] - 1);

    $sub_permitted_chars = substr($permited_chars, 0, $_SESSION['count'] + 1);
    $string = generate_string($sub_permitted_chars, $length);
    $_SESSION['string'] = $string;

    if ($_SESSION['count'] == 1) {
        $ans = prime($num);
    } else {
        if ($num % 2 == 0) {
            $num++;
        }

        $ans = rand(0, 999) < 500;
        while (prime($num) != $ans || $num % 5 == 0) {
            $num += 2;
        }

    }

    $_SESSION['number'] = $num;
    if ($ans) {
        $_SESSION['answer'] = "yes";
    } else {
        $_SESSION['answer'] = "no";
    }

}

?>

<!DOCTYPE html PUBLIC
          "-//W3C//DTD XHTML 1.0 Transitional//EN"
          "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf8" />
<title>Longest palindrome!</title>
<link href="palseq.css" rel="stylesheet" type="text/css" />
</head>
<body>

<?php
if (!isset($_SESSION['count']) || isset($_SESSION['reset'])) {
    $_SESSION['count'] = 0;
    $_SESSION['wrong'] = 0;
    $_SESSION['zero'] = microtime_float();
}
printf("Session count is: %d", $_SESSION['count']);
if (isset($_SESSION['generate']) || $_SESSION['count'] == 0) {
    generate();
}

unset($_SESSION['generate']);
unset($_SESSION['reset']);
?>

<h1>Find the longest palindrome!</h1>

<p>I'll give you a string of (up to 1000) letters
  and I need you to do one simple thing:
</p>
<p>Find the <span class="emph">least</span> possible number of letters that,
  if removed from the given string, what remains is a
  <span class="emph">palindrome</span>.
</p>
<blockquote>
  <p>For example, given the string:
    <code>bbccaddabaddacaaacdb</code>
    the correct answer is <span class="emph">5</span>.
  </p>
  <p>If one removes these five underlined letters:
    <code>b<span class="removed">b</span>ccaddabaddac<span class="removed">aaa</span>c<span class="removed">d</span>b</code>
    then the remaining string:
    <code>bccaddabaddaccb</code>
    is indeed a palindrome.  It is not possible to obtain a
    palindrome by removing fewer than five letters.
  </p>
</blockquote>
<hr/>

<p><span class="question">Question <?php echo "${_SESSION['count']}"; ?></span>:
    length <?php echo strlen($_SESSION['string']) ?>

    <!-- Also add cheat answer here -->
    <?php if ($cheat) {
    printf("<td width=\"16\">&mdash;</td>\n");
    printf("<td>if I were you, I'd answer %d</td>\n", ); //$_SESSION['answer']);
}?>

    <!-- Need to generate question here -->
    <code class="block" id="question"><?php echo "${_SESSION['string']}" ?></code>


</p>
<table border="0" cellspacing="3">
  <tr>
    <form <?php echo "action=\"$self\""; ?> id="f" name="f" method="post">
    What is the least number of characters you need to remove?
    <table border="0" cellspacing="3">
    <tr>
    <td><input type="text" class="box" name="answer" id="answer" autofocus /></td>
    <td><input type="submit" class="button" name="submit" id="submit" value="Submit!" /></td>
    </tr>
    </table>
<?php
if (isset($_POST['answer'])) {
    if ($_POST['answer'] == $_SESSION['answer']) {
        printf("<td><span class=\"correct\">RIGHT :)</span></td>\n");
    } else {
        printf("<td><span class=\"wrong\">WRONG :(</span></td>\n");
        $_SESSION['count']--;
        $_SESSION['wrong']++;
    }
    if ($_SESSION['count'] < $limit) {
        $_SESSION['generate'] = true;
        printf("<td width=\"16\">&nbsp;</td>\n");
        printf("<td><input type=\"submit\" name=\"continue\"
                             id=\"continue\" value=\"Continue!\" /></td>\n");
    } else {
        $_SESSION['reset'] = true;
    }

} else {
    //  What do we do if answer is not set?
}
?>
    </form>
  </tr>
</table>

<?php
if (isset($_SESSION['reset'])) {
    ?>
<p><span class="congratulations">Congra<a href="https://www.youtube.com/watch?v=1Bix44C1EzY">d</a>ulations!</span>
   You answered all questions!</p>
<p>It took you
  <?php printf("%0.3lf", microtime_float() - $_SESSION['zero']);?> seconds
<?php
if ($_SESSION['wrong'] == 0) {
        ?>
  and you made no mistakes.
<?php
} else if ($_SESSION['wrong'] == 1) {
        ?>
  and you made one mistake
<?php
} else {
        ?>
  and you made <?php echo "${_SESSION['wrong']}" ?> mistakes.</p>
<?php
}
    ?>
<form <?php echo "action=\"$self\""; ?> id="r" name="r" method="post">
<input type="submit" name="again" id="again" value="Play again!" />
</form>
<?php
}
?>

</body>
</html>