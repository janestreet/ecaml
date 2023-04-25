## Release v0.16.0

- Ecaml is now built and tested against Emacs 27.2 (was Emacs 26.3).

- Stop loading library `cl.el`, which is deprecated (Ecaml uses `cl-lib` now instead).

- Basic data types

    - Removed deprecated module type `Value.Type.Enum`.  Users should now use
      `Value.Type.Enum.S` instead.

    - Add `Filename.extension`, a wrapper for `file-name-extension`.

    - Add `Emacs_version.major_version`, a wrapper for variable `emacs-major-version`.

    - Add `Hook.Function.symbol`, which retrieves the name of a given hook function.

    - `Process.call_exn` now accepts an optional `?stderr` argument, which specifies what
      to do with the standard error output from the process.  The default, `Mix`, retains
      the old behavior of interleaving it with the standard output.  It can also be
      dropped with `Drop_if_ok` or redirected elsewhere with `Split`.

    - `Advice.defun_around_funcall` now raises a more informative error message when the
      advice function's arity does not match the advised function's.

- Buffers and Text

    - Add `Buffer.is_modified`, a wrapper for `buffer-modified-p`.

    - `Current_buffer.active_region` now checks `use-region-p` instead of `mark-active`.

    - Add optional arguments `max_duration` and `max_costs` for
      `Current_buffer.replace_buffer_contents`.  See the docstring for
      `replace-buffer-contents`.

    - Add `Kill_ring.current_kill_exn`, which returns the first item in the kill ring (the
      one that would be inserted by `yank`).  It can also return text copied from another
      GUI application, if available.  This is a wrapper for `current-kill`.

    - Add `Overlay.delete_property`.  This sets an overlay property to nil, as Emacs
      doesn't expose a way to actually remove the property from an overlay's plist; for
      most text properties, a nil value has the same effect as a missing one.

    - Add `Point.Property_search` module, which wraps `text-property-search-forward` and
      its required argument types.

    - Add wrappers for text property names:
      `Text.Property_name.{after_string,before_string,invisible}`, equivalent to
      `after-string`, `before-string`, and `invisible`.

    - Add `Rx.Char_class.Named`, which allows specifying a character class by name (e.g.,
      "alphabetic", "digit"), rather than by listing them in a string or specifying a range.

- Commands

    - Add type `Symbol.Disabled.t`, which allows disabled commands to include a message, such
      as to explain why a command was disabled.  This message is displayed when the user
      attempts to run the disabled command.  `defun_* ?disabled` arguments use the new type.

    - Add `Key_sequence.invoking_this_command`, a wrapper for `this-command-keys`.

- Faces

    - Add `Color.of_rgb8`, which takes arguments in the range `[0, 255]` (instead of `[0,
      65_535]` as `Color.of_rgb` does).

    - Add `Face.Attribute.Extend` variant, to handle the built-in faces in Emacs 28.

- Frames and Windows

    - `Frame.window_list` now accepts an optional `?frame` argument.  When `None`, this
      still lists the windows of the selected frame.

    - Make `Selected_window.find_file_other_window` Async, as `find_file` already was.

- Major modes

    - Add `Compilation.major_mode`, a wrapper for `compilation-mode`.

    - Add `Compilation.find_buffer`, a wrapper for `compilation-find-buffer`.

    - Add `Major_mode.Python`, a wrapper for `python-mode`.

- Minor modes

    - Add `Minor_mode.auto_fill`, a wrapper for `auto-fil-mode`

    - `Minor_mode.define_minor_mode`'s `initialize` function now receives the minor mode
      being defined as an argument; this can be used to change the behavior based on
      whether the minor mode is being enabled or disabled, or to add hooks/commands that
      may enable/disable the minor mode.

- Other

    - `Buffer.with_temp_buffer` and `Echo_area.wrap_message` now accept a
      `Sync_or_async.t` argument, instead of requiring the body to return `Deferred.t`.

    - Add `Customization.Type.Key_sequence` variant.

    - Add `Minibuffer.read_file_name`, a wrapper for `read-file-name`
