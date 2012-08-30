lilypond-ly2musicxml
====================

A Lilypond to MusicXML converter using LilyPond-engravers

30-08-2012
* Now are supported the auto closing tags (i.e. with no value and no child element).
* Added an append-child method to xml-nodes.
* The script handles now rests.

29-08-2012:
* So far can only be translated notes with following elements:
&lt;note&gt;
    &lt;pitch&gt;
        &lt;step/&gt;
        &lt;alter/&gt;?
        &lt;octave/&gt;
    &lt;/pitch&gt;
    &lt;accidental/&gt;?
    &lt;duration/&gt;
    &lt;type/&gt;
    &lt;dot/&gt;*
&lt;/note>
('?' stands for zero or one, '*' for zero to "infinity")

28-08-2012:
* Started the first steps in development of ly2musicxml converter, based on the work of Han-Wen Nienhuys <hanwen@xs4all.nl>
and Jan Nieuwenhuizen <janneke@gnu.org> (lilypond/usr/share/lilypond/current/scm/to-xml.scm) and some John Mandereau's <john.mandereau@gmail.com> ideas (see http://lists.gnu.org/archive/html/lilypond-devel/2012-08/msg00651.html).
