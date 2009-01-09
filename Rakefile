require 'rake'
require "rake/rdoctask"
require 'spec/rake/spectask'
require 'rake/gempackagetask'

require File.join(File.expand_path(File.dirname(__FILE__)), 
  'ruby','lib','couchapp')

spec = Gem::Specification.new do |s|
  s.name = "couchapp"
  s.version = CouchApp::VERSION
  s.date = "2009-01-08"
  s.summary = "Standalone CouchDB Application Development Made Simple"
  s.email = "jchris@apache.org"
  s.homepage = "http://github.com/jchris/couchapp"
  s.description = "CouchApp is a set of helpers and a jQuery plugin that conspire to get you up and running on CouchDB quickly and correctly. It brings clarity and order to the freedom of CouchDB’s document-based approach."
  s.has_rdoc = true
  s.authors = ["J Chris Anderson", "Jan Lehnardt", "Greg Borenstein"]
  s.files = %w( LICENSE README.md Rakefile ruby/bin/couchapp) + 
    Dir["app-template/**/*.*"] + Dir["ruby/{bin,lib,spec}/**/*.*"] -
    Dir["ruby/spec/scratch"] - Dir["ruby/spec/scratch/**/*"]
  s.extra_rdoc_files = %w( README.md LICENSE )
  s.require_path = "ruby/lib"
  s.bindir = 'ruby/bin'
  s.executables << 'couchapp'
  
  
  dependencies = "

    json
    JSON implementation for Ruby using fast compiled bindings
    http://json.rubyforge.org/
    >=1.1.3

    json_pure
    JSON implementation for Ruby using pure ruby processing, which is required for JRuby
    http://json.rubyforge.org/
    >=1.1.3

    couchrest 
    Lean and RESTful interface to CouchDB.
    http://github.com/jchris/couchrest
    >= 0.11.0

    mime-types
    MIME Types manages a MIME Content-Type that will return the Content-Type for a given filename.
    http://mime-types.rubyforge.org/
    >=1.15.0

  "
  dependencies = dependencies.strip.gsub(/^ +/,'').split(/\n\n/).map{|x|x.split(/\n/)}

  dependencies.each{|d|
    name, uri, desc, version, z = d
    s.add_dependency(name,version)
  }

end

::Rake::GemPackageTask.new(spec) { |p| p.gem_spec = spec }

desc "Run all specs"
Spec::Rake::SpecTask.new('spec') do |t|
	t.spec_files = FileList['ruby/spec/**/*_spec.rb']
end

desc "Print specdocs"
Spec::Rake::SpecTask.new(:doc) do |t|
	t.spec_opts = ["--format", "specdoc"]
	t.spec_files = FileList['ruby/spec/*_spec.rb']
end

desc "Generate the rdoc"
Rake::RDocTask.new do |rdoc|
  files = ["README.md", "LICENSE", "ruby/lib/**/*.rb"]
  rdoc.rdoc_files.add(files)
  rdoc.main = "README.md"
  rdoc.title = "CouchApp: Standalone CouchDB Application Development Made Simple"
end


desc "Run the rspec"
task :default => :spec


desc "Update Github Gemspec"
task :gemspec do
  skip_fields = %w(new_platform original_platform)
  integer_fields = %w(specification_version)

  result = "Gem::Specification.new do |s|\n"
  spec.instance_variables.each do |ivar|
    value = spec.instance_variable_get(ivar)
    name  = ivar.split("@").last
    next if skip_fields.include?(name) || value.nil? || value == "" || (value.respond_to?(:empty?) && value.empty?)
    if name == "dependencies"
      value.each do |d|
        dep, *ver = d.to_s.split(" ")
        result <<  "  s.add_dependency #{dep.inspect}, [#{ /\(([^\,]*)/ . match(ver.join(" "))[1].inspect}]\n"
      end
    else        
      case value
      when Array
        value =  name != "files" ? value.inspect : value.inspect.split(",").join(",\n")
      when Fixnum
        # leave as-is
      when String
        value = value.to_i if integer_fields.include?(name)
        value = value.inspect
      else
        value = value.to_s.inspect
      end
      result << "  s.#{name} = #{value}\n"
    end
  end
  result << "end"
  File.open(File.join(File.dirname(__FILE__), "#{spec.name}.gemspec"), "w"){|f| f << result}
end