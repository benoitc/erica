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
    it "should create a show directory" do
      @files.select{|x|x =~ /show\Z/}.length.should == 1
    end
    it "should create a forms and libs" do
      @files.select{|x|x =~ /example-show.js/}.length.should == 1
      @files.select{|x|x =~ /example.html/}.length.should == 1
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
      @doc['show']['docs']['example-show'].should_not match(/\"helpers\"/)
    end
    it "should create view for all the views" do
      @doc['views']['more']['map'].should match(/moremap/)
    end
    it "should create the index" do
      @doc['_attachments']['index.html']["content_type"].should == 'text/html'
    end
    it "should push the forms" do
      @doc['show']['docs']['example-show'].should match(/Generated CouchApp Form Template/)
    end
    it "should allow deeper includes" do
      @doc['show']['docs']['example-show'].should_not match(/\"helpers\"/)
    end
    it "deep requires" do
      @doc['show']['docs']['example-show'].should_not match(/\"template\"/)
      @doc['show']['docs']['example-show'].should match(/Resig/)
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


end

