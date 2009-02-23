require 'rake'

begin
  require 'spec/rake/spectask'
rescue LoadError
  puts <<-EOS
To use rspec for testing you must install rspec gem:
    gem install rspec
EOS
  exit(0)
end

desc "Run Ruby specs on the Python version" 
task :python do
  system "ruby ruby/spec/couchapp_spec.rb -- python"
end

desc "Run the rspec"
task :default => :python
