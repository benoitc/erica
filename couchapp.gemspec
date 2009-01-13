Gem::Specification.new do |s|
  s.extra_rdoc_files = ["README.md", "LICENSE"]
  s.date = "Tue Jan 13 00:00:00 -0800 2009"
  s.executables = ["couchapp"]
  s.authors = ["J Chris Anderson", "Jan Lehnardt", "Greg Borenstein"]
  s.required_rubygems_version = ">= 0"
  s.version = "0.1.4"
  s.files = ["LICENSE",
 "README.md",
 "Rakefile",
 "ruby/bin/couchapp",
 "app-template/_attachments/index.html",
 "app-template/_attachments/style/main.css",
 "app-template/foo/bar.txt",
 "app-template/lib/helpers/math.js",
 "app-template/lib/helpers/template.js",
 "app-template/lib/templates/example.html",
 "app-template/show/docs/example-show.js",
 "app-template/views/example/map.js",
 "app-template/views/example/reduce.js",
 "ruby/lib/couchapp.rb",
 "ruby/lib/file_manager.rb",
 "ruby/spec/couchapp_spec.rb",
 "ruby/spec/file_manager_spec.rb",
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
  s.add_dependency "couchrest", [">= 0.12.1"]
  s.require_paths = ["ruby/lib"]
end