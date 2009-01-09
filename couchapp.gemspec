Gem::Specification.new do |s|
  s.extra_rdoc_files = ["README.md", "LICENSE"]
  s.date = "Thu Jan 08 00:00:00 -0800 2009"
  s.executables = ["couchapp"]
  s.authors = ["J Chris Anderson", "Jan Lehnardt", "Greg Borenstein"]
  s.required_rubygems_version = ">= 0"
  s.version = "0.1.1"
  s.files = ["LICENSE",
 "README.md",
 "Rakefile",
 "ruby/bin/couchapp",
 "ruby/lib/couchapp.rb",
 "ruby/lib/file_manager.rb",
 "ruby/spec/couchapp_spec.rb",
 "ruby/spec/scratch/couchapp-test",
 "ruby/spec/scratch/couchapp-test/my-app",
 "ruby/spec/scratch/couchapp-test/my-app/_attachments",
 "ruby/spec/scratch/couchapp-test/my-app/_attachments/index.html",
 "ruby/spec/scratch/couchapp-test/my-app/foo",
 "ruby/spec/scratch/couchapp-test/my-app/foo/bar.txt",
 "ruby/spec/scratch/couchapp-test/my-app/forms",
 "ruby/spec/scratch/couchapp-test/my-app/forms/example-form.js",
 "ruby/spec/scratch/couchapp-test/my-app/lib",
 "ruby/spec/scratch/couchapp-test/my-app/lib/helpers",
 "ruby/spec/scratch/couchapp-test/my-app/lib/helpers/math.js",
 "ruby/spec/scratch/couchapp-test/my-app/lib/helpers/template.js",
 "ruby/spec/scratch/couchapp-test/my-app/lib/templates",
 "ruby/spec/scratch/couchapp-test/my-app/lib/templates/example.html",
 "ruby/spec/scratch/couchapp-test/my-app/views",
 "ruby/spec/scratch/couchapp-test/my-app/views/example",
 "ruby/spec/scratch/couchapp-test/my-app/views/example/map.js",
 "ruby/spec/scratch/couchapp-test/my-app/views/example/reduce.js",
 "ruby/spec/spec.opts",
 "ruby/spec/spec_helper.rb"]
  s.has_rdoc = "true"
  s.specification_version = 2
  s.loaded = "false"
  s.email = "jchris@apache.org"
  s.name = "couchapp"
  s.required_ruby_version = ">= 0"
  s.bindir = "ruby/bin"
  s.rubygems_version = "1.2.0"
  s.homepage = "http://github.com/jchris/couchapp"
  s.platform = "ruby"
  s.summary = "Standalone CouchDB Application Development Made Simple"
  s.description = "CouchApp is a set of helpers and a jQuery plugin that conspire to get you up and running on CouchDB quickly and correctly. It brings clarity and order to the freedom of CouchDB\342\200\231s document-based approach."
  s.add_dependency "json", [">= 1.1.3"]
  s.add_dependency "json_pure", [">= 1.1.3"]
  s.add_dependency "couchrest", [">= 0.11.0"]
  s.add_dependency "mime-types", [">= 1.15.0"]
  s.require_paths = ["ruby/lib"]
end