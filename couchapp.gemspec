Gem::Specification.new do |s|
  s.extra_rdoc_files = ["README.md", "LICENSE"]
  s.date = "Thu Jan 08 00:00:00 -0800 2009"
  s.executables = ["couchapp"]
  s.authors = ["J Chris Anderson", "Jan Lehnardt", "Greg Borenstein"]
  s.required_rubygems_version = ">= 0"
  s.version = "0.1.2"
  s.files = ["LICENSE",
 "README.md",
 "Rakefile",
 "app-template/_attachments",
 "app-template/_attachments/index.html",
 "app-template/foo",
 "app-template/foo/bar.txt",
 "app-template/forms",
 "app-template/forms/example-form.js",
 "app-template/lib",
 "app-template/lib/helpers",
 "app-template/lib/helpers/math.js",
 "app-template/lib/helpers/template.js",
 "app-template/lib/templates",
 "app-template/lib/templates/example.html",
 "app-template/views",
 "app-template/views/example",
 "app-template/views/example/map.js",
 "app-template/views/example/reduce.js",
 "ruby/bin/couchapp",
 "ruby/lib/couchapp.rb",
 "ruby/lib/file_manager.rb",
 "ruby/spec/couchapp_spec.rb",
 "ruby/spec/file_manager_spec.rb",
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
 "ruby/spec/scratch/generated-app",
 "ruby/spec/scratch/generated-app/app-template",
 "ruby/spec/scratch/generated-app/app-template/_attachments",
 "ruby/spec/scratch/generated-app/app-template/_attachments/index.html",
 "ruby/spec/scratch/generated-app/app-template/foo",
 "ruby/spec/scratch/generated-app/app-template/foo/bar.txt",
 "ruby/spec/scratch/generated-app/app-template/forms",
 "ruby/spec/scratch/generated-app/app-template/forms/example-form.js",
 "ruby/spec/scratch/generated-app/app-template/lib",
 "ruby/spec/scratch/generated-app/app-template/lib/helpers",
 "ruby/spec/scratch/generated-app/app-template/lib/helpers/math.js",
 "ruby/spec/scratch/generated-app/app-template/lib/helpers/template.js",
 "ruby/spec/scratch/generated-app/app-template/lib/templates",
 "ruby/spec/scratch/generated-app/app-template/lib/templates/example.html",
 "ruby/spec/scratch/generated-app/app-template/views",
 "ruby/spec/scratch/generated-app/app-template/views/example",
 "ruby/spec/scratch/generated-app/app-template/views/example/map.js",
 "ruby/spec/scratch/generated-app/app-template/views/example/reduce.js",
 "ruby/spec/scratch/generated-app/forms",
 "ruby/spec/scratch/generated-app/test.json",
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