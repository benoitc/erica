require File.dirname(__FILE__) + '/spec_helper'

describe "couchapp" do
  before(:all) do
    @fixdir = SCRATCH_PATH + '/couchapp-test'
    `rm -rf #{@fixdir}`
    `mkdir -p #{@fixdir}`
    @run = "cd #{@fixdir} && #{COUCHAPP}"
  end

  describe "--help" do
    it "should output the opts" do
      `#{@run} --help`.should match(/Usage/)
    end
  end

  describe "generate my-app" do
    before(:all) do
      `#{@run} generate my-app`.should match(/generating/i)      
      @files = Dir["#{@fixdir}/my-app/**/*"]
    end
    it "should create an app directory" do
      @files.select{|x|x =~ /my-app/}.should_not be_empty
    end
    it "should create a views directory" do
      @files.select{|x|x =~ /views\Z/}.length.should == 1
    end
    it "should create an _attachments directory" do
      @files.select{|x|x =~ /_attachments\Z/}.length.should == 1
      @files.select{|x|x =~ /_attachments\/index.html/}.length.should == 1
    end
    it "should create a shows directory" do
      @files.select{|x|x =~ /shows\Z/}.length.should == 1
      @files.select{|x|x =~ /example-show.js/}.length.should == 1
    end
    it "should create a libs" do
      @files.select{|x|x =~ /templates\/example.html/}.length.should == 1
    end
    it "should show deep attachment capabilities" do
      @files.select{|x|x =~ /main.css/}.
        first.should include('style')
    end
  end
  
  describe "push my-app #{TESTDB}" do
    before(:all) do
      @cr = CouchRest.new(COUCHHOST)
      @db = @cr.database(TESTDB)
      @db.delete! rescue nil
      @db = @cr.create_db(TESTDB) rescue nil
      `#{@run} generate my-app`
      `mkdir -p #{@fixdir}/my-app/views/more`
      `echo 'moremap' > #{@fixdir}/my-app/views/more/map.js`
      `#{@run} push my-app #{TESTDB}`
      @doc = @db.get("_design/my-app")
    end
    it "should create the design document with the app name" do
      lambda{@db.get("_design/my-app")}.should_not raise_error
    end
    it "should create the views" do
      @doc['views']['example']['map'].should match(/function/)
    end
    it "should create the view libs" do
      @doc['views']['example']['map'].should match(/stddev/)
      @doc['shows']['example-show'].should match(/ejohn\.org/)
    end
    it "should create view for all the views" do
      @doc['views']['more']['map'].should match(/moremap/)
    end
    it "should create the index" do
      @doc['_attachments']['index.html']["content_type"].should == 'text/html'
    end
    it "should create the manifest" do
      @doc['app_meta']['manifest'][0].should match(/foo\//)
    end
    it "should push and macro the doc shows" do
      @doc['shows']['example-show'].should match(/Generated CouchApp Form Template/)
    end
    it "should push and macro the view lists" do
      @doc['lists']['feed'].should match(/Test XML Feed/)
    end
    it "should allow deeper includes" do
      @doc['shows']['example-show'].should_not match(/\"helpers\"/)
    end
    it "deep require macros" do
      @doc['shows']['example-show'].should_not match(/\"template\"/)
      @doc['shows']['example-show'].should match(/Resig/)
    end
    
    it "should push to other design docs" do
      lambda{@db.get("_design/my-design")}.should raise_error
      `#{@run} push my-app my-design #{TESTDB}`
      lambda{@db.get("_design/my-design")}.should_not raise_error
    end
    
    # cd should be last
    it "should work on . dir as well" do
      @db = reset_test_db!
      lambda{@db.get("_design/my-app")}.should raise_error
      `cd #{@fixdir}/my-app && #{COUCHAPP} push . #{TESTDB}`
      lambda{@db.get("_design/my-app")}.should_not raise_error
    end
  end

  describe "clone #{TESTDB}/_design/my-app" do
    before(:all) do
      @cr = CouchRest.new(COUCHHOST)
      @db = @cr.database(TESTDB)
      @db.delete! rescue nil
      @db = @cr.create_db(TESTDB) rescue nil
      `#{@run} generate my-app`
      `#{@run} push my-app #{TESTDB}`
      @doc = @db.get("_design/my-app")
    end

    describe "normally" do
      before(:all) do
        `rm -rf #{@fixdir}/my-app`
        `cd #{@fixdir} && #{COUCHAPP} clone http://127.0.0.1:5984/#{TESTDB}/_design/my-app`
      end
      it "should clone the views" do
        File.exist?("#{@fixdir}/my-app/views").should == true
      end
      it "should create foo/bar.txt file" do
        File.exist?("#{@fixdir}/my-app/foo/bar.txt").should == true
      end
      it "should create lib/helpers/math.js file" do
        File.exist?("#{@fixdir}/my-app/lib/helpers/math.js").should == true
      end
    end

    it "should work when design doc is edited manually" do
      @doc['test.txt'] = "essai"
      @doc.save()
      `rm -rf #{@fixdir}/my-app`
      `cd #{@fixdir} && #{COUCHAPP} clone http://127.0.0.1:5984/#{TESTDB}/_design/my-app`
      File.exist?("#{@fixdir}/my-app/test.txt").should == true
    end
    it "should work when a view is added manually" do
      @doc["views"]["more"] = { "map" => "function(doc) { emit(null, doc); }" }
      @doc.save()
      `rm -rf #{@fixdir}/my-app`
      `cd #{@fixdir} && #{COUCHAPP} clone http://127.0.0.1:5984/#{TESTDB}/_design/my-app`
      File.exist?("#{@fixdir}/my-app/views/more/map.js").should == true
    end
    it "should create view even if file dir is missing in manifest" do
      @doc['app_meta']["manifest"].delete_at(12)
      @doc.save()
      `rm -rf #{@fixdir}/my-app`
      `cd #{@fixdir} && #{COUCHAPP} clone http://127.0.0.1:5984/#{TESTDB}/_design/my-app`
      File.exist?("#{@fixdir}/my-app/views/example/map.js").should == true
    end
    it "should work without manifest" do
      @doc['app_meta'].delete('manifest')
      @doc.save()
      `rm -rf #{@fixdir}/my-app`
      `cd #{@fixdir} && #{COUCHAPP} clone http://127.0.0.1:5984/#{TESTDB}/_design/my-app`
      File.exist?("#{@fixdir}/my-app/views").should == true
    end
    it "should create foo/bar.txt without manifest" do
      @doc['app_meta'].delete('manifest')
      @doc.save()
      `rm -rf #{@fixdir}/my-app`
      `cd #{@fixdir} && #{COUCHAPP} clone http://127.0.0.1:5984/#{TESTDB}/_design/my-app`
      File.exist?("#{@fixdir}/my-app/foo/bar.txt").should == true
    end
    it "should create lib/helpers.json without manifest" do
      @doc['app_meta'].delete('manifest')
      @doc.save()
      `rm -rf #{@fixdir}/my-app`
      `cd #{@fixdir} && #{COUCHAPP} clone http://127.0.0.1:5984/#{TESTDB}/_design/my-app`
      File.exist?("#{@fixdir}/my-app/lib/helpers.json").should == true
    end
    

  end
end

