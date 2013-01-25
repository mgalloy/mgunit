mgunit
======

Simple testing in IDL
---------------------

`mgunit` is a unit testing framework modeled on other `xUnit testing frameworks
<http://en.wikipedia.org/wiki/XUnit>`_. The goal is to allow easy creation and
reporting of results of tests, while still allowing for many different testing
situations. Simple naming conventions replace formal creation of hierarchies
and specification of tests. This allows test suites to be created with a
minimum of code beyond the actual code of the tests themselves.

The basic structure of `mgunit` is that tests are created by subclassing
``MGutTestCase``, tests can be grouped together into suites for convenience by
subclassing ``MGutTestSuite``, and tests are run my calling ``MGUNIT``. Various
convenience routines are included, such as the ``ASSERT`` routine which is
useful for making an assertion during a test.

See "Using mgunit" in the ``docs/`` directory for more details about using
`mgunit`.

See the `Releases <https://github.com/mgalloy/mgunit/wiki/Releases>`_ page in
the wiki for downloads.
