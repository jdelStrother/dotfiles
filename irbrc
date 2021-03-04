IRB.conf[:SAVE_HISTORY] = 2000
IRB.conf[:HISTORY_FILE] = '~/.irb-history'
# multiline is distressingly slow in ruby<3
# https://github.com/ruby/irb/issues/43
IRB.conf[:USE_MULTILINE] = RUBY_VERSION >= '3.0.0'
