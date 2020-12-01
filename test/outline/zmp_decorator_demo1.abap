*&---------------------------------------------------------------------*
*& Report ZMP_DECORATOR_DEMO1
*&---------------------------------------------------------------------*
*&
*& Purpose.  Decorator design pattern
*&
*& TODO add docu
*&---------------------------------------------------------------------*
report zmp_decorator_demo1.

interface Widget.
  methods draw.
endinterface.

class TextField definition.
public section.
  interfaces: Widget.

  methods constructor importing i_Width type i i_Height type i.
private section.
  data width type i.
  data height type i.
endclass.

class TextField implementation.
  method constructor.
    width = i_Width.
    height = i_Height.
  endmethod.

  method Widget~draw.
    write: / 'TextField: ', width, ', ', height.
  endmethod.
endclass.

class Decorator definition.
public section.
  interfaces Widget.

  " crucial are the alias named draw for the interface method
  " and the redefinition of this method in the subclasses
  aliases draw for Widget~draw.

  methods constructor importing i_Widget type ref to Widget.
private section.
  data wid type ref to Widget.
endclass.

class Decorator implementation.
  method constructor.
    wid = i_Widget.
  endmethod.

  method draw.
   wid->draw( ).
  endmethod.
endclass.

class BorderDecorator definition inheriting from Decorator.
public section.

  methods constructor importing i_Widget type ref to Widget.
  methods draw redefinition.
endclass.

class BorderDecorator implementation.
  method constructor.
    super->constructor( i_Widget ).
  endmethod.

  method draw.
    super->draw( ).
    write / '   BorderDecorator'.
  endmethod.
endclass.

class ScrollDecorator definition inheriting from Decorator.
public section.

  methods constructor importing i_Widget type ref to Widget.
  methods draw redefinition.
endclass.

class ScrollDecorator implementation.
  method constructor.
    super->constructor( i_Widget ).
  endmethod.

  method draw.
    super->draw( ).
    write / '   ScrollDecorator'.
  endmethod.
endclass.

start-of-selection.

data aWidget type ref to Widget.
aWidget = new BorderDecorator(
            new BorderDecorator(
              new ScrollDecorator(
                new TextField( i_Width = 80 i_Height = 24 ) ) ) ).
aWidget->draw( ).

types:
  begin of ty_data,
    i type i,
    s type string,
  end of ty_data,
  tty_data type hashed table of ty_data with unique key i.

data(items) = value tty_data( ( i = 1 s = 'Test' )
                              ( i = 2 s = 'Hello World' )
                              ( i = 3 s = 'xyz' ) ).

data(filtered_data) = filter #( items except where i = 1 ).

loop at filtered_data into data(filtered_data_row).
  write / filtered_data_row-i.
  write filtered_data_row-s.
endloop.

end-of-selection.
