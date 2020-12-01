report zsmp_etagstemplate.

class main definition abstract final.
public section.
  class-methods run.

private section.
  class-methods setup.

  class-methods m_0001.
endclass.

class main implementation.
  method run.
    setup( ).

    m_0001( ).
  endmethod.

  method setup.
  endmethod. " setup

  method m_0001.
  endmethod. " m_0001
endclass. " main

start-of-selection.
main=>run( ).
end-of-selection.
