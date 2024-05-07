## Release v0.17.0

- Ecaml is now built and tested against Emacs 29.2 (was Emacs 27.2).

- Basic data types

    - Removed unused module **type** `Customization.Enum`

    - `Customization.Type.Option` is now an inline record with self-documenting fields.  It
      corresponds to the Elisp customization type `'(choice (const :tag "documentation for
      nil" nil) <some-other-type>)`.

    - `Defun.defalias` now allows the definition to be any value, not just a symbol, as
      the Elisp `defalias` allows.

    - Removed `Symbol.set_function`.  Use `Defun.defalias` instead, which handles
      load-history, documentation, advice, etc. properly.

    - `Hook` now supports binding special hooks, including those whose functions return
      meaningful values.  `Hook.t`, `Hook_type.t`, and `Hook.Function.t` now have an
      additional type parameter to track the return type of the hook functions.

    - Add variant `Hook_type.Query_function` for things like `kill-buffer-query-functions`.

    - Add `Progress_reporter` module, with bindings and helper functions for echo area
      [progress reporters](https://www.gnu.org/software/emacs/manual/html_node/elisp/Progress.html)

    - Add `Text.of_utf8_bytes_replacing_invalid`, similar to `Text.of_utf8_bytes` but with
      error-handling if the OCaml string contains malformed UTF-8 sequences

    - Add `Text.Property_name.help_echo` and `Text.Property_name.kbd_help` bindings for Emacs
      mouse-over and idle help display functionality.

    - Add `Timer.run_after_idle` and `Timer.run_after_idle_i` for creating idle timers
      (`run-with-idle-timer`)

- Buffers

    - New function `Buffer.find_by` finds a live buffer using a predicate function

    - `Current_buffer.replace_buffer_contents` is assumed to always be available (requires
      Emacs 26+).

    - Removed function `Current_buffer.describe_mode`

    - Add `Mode_line.Format.header_line_in_buffer`, a binding for `header-line-format` (like
      `mode-line-format`, but for the buffer-local header line).

    - Add `Mode_line.Format.empty` and `Mode_line.Format.string_verbatim` helpers to make
      mode-line format constructs.

- Commands and user interaction

    - Removed `Command.quit_flag` binding for `quit-flag`.  Ecaml now calls the
      `process_input` module function to poll for whether a blocking Async operation should be
      interrupted, and preserves the original `quit` signal instead of replacing it with an
      OCaml exception.

      If you need to set this variable for whatever reason, you can use
      `Command.Private.request_quit`, but this is not useful outside of tests.

    - `Completing.read` and `Completing.read_multiple` now accept `collection` parameter as
      `string list` instead of a variant type.  The Elisp value variant was almost never used.

    - Add `Prefix` variant to `Defun.Interactive.t` for numerical prefix arguments.  This
      corresponds to the `p` interactive specifier in Elisp.

    - Add `Browse_url.browser-function`, a binding for the `browse-url-browser-function` customization.

    - Add binding `User.init_file` for `user-init-file` variable.  It is `None` if Emacs was
      started with `-q` or `--no-init-file` options.

- Major modes

    - Add `Major_mode.Lisp_data` for `lisp-data-mode`.  `Major_mode.Lisp`, which is now
      unused, has been deleted.

    - Reworked the `Tabulated_list` API.  Creating a `Tabulated_list.t` now requires
      specifying a function to extract a non-empty string ID for each record, rather than an
      arbitrary ID type.  This change was required to provide a custom sorting function for
      each column, instead of just the native Emacs string comparison function.

    - Records (not just IDs) are now stored in the buffer itself and can be extracted by
      calling `Tabulated_list.get_record_at_point_exn`.

- System

    - Add optional argument to `Directory.files_recursively`: `?ignore_unreadable_dirs`
      defaults to false and corresponds to the `PREDICATE` argument of
      `directory-files-recursively`.

    - Update the type of `Filename.expand`'s `~in_dir` argument.
      `Default_directory_in_current_buffer` is equivalent to omitting the second argument to
      `expand-file-name`.

    - Add optional arguments to `Process.create`: `coding`, `query_before_exit`, and `stderr`.
      These correspond to the `:coding`, `:noquery` (inverted) and `:stderr` arguments to
      `make-process`.

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
