use strict;
use warnings;
use HTML::TreeBuilder;
use Data::Dumper;
use File::Basename;

#
# Global variables & forward declarations
#
my $ns = "vw";				# namespace prefix: vSphere SDK wrapper
my $prefix = "${ns}1__";	# follow gSOAP convention
my %typeinfo_html;			# type information defined in HTML help files
my %typeinfo_stub;			# type information defined in generated stub file
my %name_to_C_type;			# name defined in HTML to type defined in stub file

sub get_text($);
sub find_table($$);
sub find_method($$);

#
# Parse HTML help pages of vim/vSphere SDK to temporary file
#
sub get_page_content($)
{
	my $page = shift;
	my $tree = HTML::TreeBuilder->new;
	$tree->parse_file($page) || die $!;

	my @nodes = $tree->guts();
	my $content = \@{$nodes[0]{_body}{_content}};
	if ((get_text $content->[3]) !~ /Data Object|Managed Object|Enum|Fault/)
	{
		$tree->delete;
		undef;
	}
	return ($content, $tree);
}

sub get_text($)
{
	my $cont = shift;
	if (not ref $cont)
	{
		return $cont;
	}
	elsif (UNIVERSAL::isa($cont, "HASH"))
	{
		return "" if not exists $cont->{_content};
		return get_text $cont->{_content};
	}
	elsif (UNIVERSAL::isa($cont, "ARRAY"))
	{
		my $text = "";
		$text .= get_text $_ for @{$cont};
		return $text;
	}
	else
	{
		warn "invalid parameter: ", ref $cont, "\n";
	}
	return "";
}

sub get_class_name($)
{
	my $content = shift;
	return get_text $content->[3];
}

sub get_desc_helper($$)
{
	my $content = shift;
	my $idx = shift;
	my $desc = get_text $content->[$idx];
	while ($idx < scalar @{$content} - 1)
	{
		++$idx;
		last if ref $content->[$idx] and $content->[$idx]{_tag} eq "p";
		$desc .= get_text $content->[$idx];
	}
	return $desc;
}

sub get_class_desc($)
{
	my $content = shift;
	my $index = 7;
	++$index if $content->[3]{_content}[0] =~ /Managed Object/;
	++$index if $content->[$index] =~ /NOTE:/;

	my $desc = get_desc_helper $content, $index;
	$desc = "<NONE>" if not $desc;
	return $desc;
}

sub get_class_parent($)
{
	my $content = shift;
	for (0..scalar @{$content->[4]{_content}} - 1)
	{
		my $value = get_text $content->[4]{_content}[$_] ;
		return get_text $content->[4]{_content}[$_+1] if $value =~ /^\s*Extends\s*$/;
	}
	return "<NONE>";
}

sub find_table($$)
{
	my $content = shift;
	my $name = shift;
	for my $i (1..scalar @{$content} - 1)
	{
		if (ref $content->[$i] and exists $content->[$i]{_content})
		{
			my $tmp = $content->[$i]{_content}[0];
			next if not defined $tmp;
			if ($tmp =~ /^\s*$name\s*$/)
			{
				warn "This is not a table: $name!"
					if $content->[$i + 1]{_tag} ne "table";
				return $content->[$i + 1]{_content};
			}
		}
	}
	
	# For some files (lised below), the format are not same to most of others.
	# We need find in the last element. Bug??
	#
	# Files that have this issue: 
	#	vim.FileManager.html
	#	vim.VirtualApp.html
	#	vim.VirtualDiskManager.html
	#	
	return find_table $content->[-1]{_content}, $name
		if ref $content->[-1] and(ref $content->[-1]{_content}) =~ /ARRAY/;
	return undef;
}

sub get_class_props($)
{
	my $content = shift;
	my $prop_table = find_table $content, "Properties";
	return undef if not $prop_table;

	my @props;
	my $has_prop = undef;
	for (@{$prop_table})
	{
		# Exit if property item is over
		last if scalar @{$_->{_content}} != 3;

		# Skip the table header: Name, Type & Description
		next if get_text($_->{_content}[0]) =~ /^\s*Name\s*$/i
			and get_text($_->{_content}[1]) =~ /^\s*Type\s*$/i
			and get_text($_->{_content}[2]) =~ /^\s*Description\s*$/i;

		# Each talbe row is a property
		my %prop;
		$prop{name} = get_text $_->{_content}[0];
		$prop{type} = get_text $_->{_content}[1];
		$prop{description} = get_text $_->{_content}[2];
		push @props, \%prop;
		$has_prop = 1;
	}
	return \@props if defined $has_prop;
	undef;
}

sub get_class_method_names($)
{
	my $content = shift;
	my $method_table = find_table $content, "Methods";
	return undef if not $method_table;
	my $ms = get_text $method_table->[1]{_content};
	my @names = split /, /, $ms;
	return \@names if not (scalar @names == 1 and $names[0] =~ /^\s*None\s*$/);
	undef;
}

sub find_method($$)
{
	my $content = shift;
	my $name = shift;
	my $idx = -1;
	for (0 .. scalar @{$content} - 1)
	{
		if (ref $content->[$_]
			and defined $content->[$_]{_tag}
			and $content->[$_]{_tag} eq 'h1'
			and get_text $content->[$_] eq $name)
		{
			# Find the method definition!
			$idx = $_;
			last;
		}
	}
	
	# Special cases, as said in find_table
	return find_method $content->[-1]{_content}, $name if $idx == -1
		and ref $content->[-1]
		and (ref $content->[-1]{_content}) =~ /ARRAY/;
	return ($idx, $content);
}

sub get_class_method($$)
{
	my ($idx, $content) = find_method $_[0], $_[1];
	return undef if $idx == -1;

	my %method;
	$method{name} = $_[1];
	$method{description} = get_text $content->[$idx + 2];
	for (;;)
	{
		++$idx;
		last if ($idx > scalar @{$content} - 1)
			or (ref $content->[$idx]
				and defined $content->[$idx]{_tag}
				and $content->[$idx]{_tag} eq 'h1');

		my $text = get_text $content->[$idx];
		if (defined $text)
		{
			if ($text =~ /\s*Parameters\s*/)
			{
				#
				++$idx;
				my $para_table = $content->[$idx]{_content};
				my @paras;
				for (@{$para_table})
				{
					# Skip the table header: Name, Type & Description
					next if get_text($_->{_content}[0]) eq "Name"
						and get_text($_->{_content}[1]) eq "Type"
						and get_text($_->{_content}[2]) eq "Description";
					
					# Skip _this parameter
					next if get_text($_->{_content}[0]) eq "_this";
			
					# Then each row is a parameter
					my %para;
					$para{name} = get_text $_->{_content}[0];
					$para{type} = get_text $_->{_content}[1];
					$para{description} = get_text $_->{_content}[2];
					push @paras, \%para;
				}
				$method{params} = \@paras;
			}
			elsif ($text =~ /\s*Return Value\s*/)
			{
				# Return value
				++$idx;
				my $ret_value_table = $content->[$idx]{_content};
				$method{ret_type} = get_text $ret_value_table->[1]{_content}[0];
			}
			elsif ($text =~ /\s*Faults\s*/)
			{
				# Throw faults, not used now
				++$idx;
			}
			else
			{
				# others we don't care by now
			}
		}
	}
	return \%method;
}

sub parse_html_doc($$)
{
	my $page = shift;
	my $file = fileparse $page;
	return if $file =~ /^[^\.]+?.html$/; # skip useless files 
	my ($content, $tree) = get_page_content $page;
	return if not $content;
	
	my $outh = shift;
	my $old  = select $outh;
	my $is_file_manager_case = undef;

	print <<DOC;
########################################
$file
DOC
	print "Name: ", get_class_name $content, "\n";
	print "Desc: ", get_class_desc $content, "\n";
	print "Extends: ", get_class_parent $content, "\n";

	my $props = get_class_props $content;
	if (defined $props)
	{
		print "PROPS:\n";
		for (0 .. scalar @{$props} - 1)
		{
			# Remove " P" suffix of name
			my $name = $props->[$_]{name};
			$name =~ s/ P$//;

			# Remove "ManagedObjectReference[] to a " prefix of type
			my $type = $props->[$_]{type};

			# Repalce "\n" wiht space in description
			my $desc = $props->[$_]{description};
			$desc =~ s/\n/ /g;
			
			$type =~ s/ManagedObjectReference(\[\])? to a //;
			print " => $name ($type): $desc\n";
		}
	}
	else
	{
		print "PROPS: <NONE>\n"
	}
	
	my $methods = get_class_method_names $content;
	if (defined $methods)
	{
		print "METHODS:\n";
		for my $func_name (@{$methods})
		{
			my $method_def = get_class_method $content, $func_name;
			if (not defined $method_def)
			{
				warn "Cannot find the definition: $file:$func_name\n";
				next;
			}
			my $params = $method_def->{params};
			my @decls;
			for my $param (@{$params})
			{
				# Remove " P" and "*" suffix of name
				my $name = $param->{name};
				$name =~ s/ P$//;
				$name =~ s/\*$//;

				# Remove "ManagedObjectReference[] to a " prefix of typr
				my $type = $param->{type};
				$type =~ s/^ManagedObjectReference(\[\])? to a //;
				
				push @decls, "$type $name";
			}
			my $param_list = join ", ", @decls;
			my $ret_type = $method_def->{ret_type};
			$ret_type = "void" if $ret_type =~ /None/;
			print " #$func_name($param_list) -> $ret_type\n";
		}
	}
	else
	{
		print "METHODS: <NONE>\n";
	}
	
	print "[END]\n";
	select $old;
	$tree->delete;
}

#
# parse stub header that is generated by gSOAP to read the really symbol
#
sub parse_stub_header($)
{
	my $stub = shift;
	open DEFINE, "<$stub" or die "cannot open $stub: $!";
	# structure, parent, kids

	my $in_class;
	my $class_name;
	my $desc_name;
	for (<DEFINE>)
	{
		chomp;
		s/^\s+|\s+$//;
		if (m!/\* ${ns}\d:(\w+) \*/!)
		{
			$desc_name = $1;
		}
		if (/class SOAP_CMAC (\w+)( : public (\w+))?/)
		{
			$in_class = 1;
			$class_name = $1;
			$typeinfo_stub{$1}{"parent"} = $3 if defined $2;
		}
		elsif (/^\s*\};/)
		{
			$in_class = undef;
			$class_name = undef;
		}
		elsif ($in_class
			   and /^\s*(struct|class|enum)?\s*(\w+\s*\*{0,})\s*(\w+);(.+)?/)
		{
			my %kid;
			$kid{"type"} = $2;
			$kid{"name"} = $3;
			$kid{"must"} = defined $4 and $4 =~ /required/;
			push @{$typeinfo_stub{$class_name}{"kids"}}, \%kid;
			$name_to_C_type{$desc_name} = $class_name if defined $desc_name;
		}
		elsif (/enum (\w+) \{(.+?)\}/)
		{
			my %kid;
			$kid{"type"} = $1;
			my %enums;
			for (split /,\s*/, $2)
			{
				my ($v, $n) = split /\s*=\s*/, $_;
				$enums{$n} = $' if $v =~ /$kid{"type"}__/;
			}
			$kid{"values"} = \%enums;
			push @{$typeinfo_stub{$1}{"kids"}}, \%kid;
		}
		else
		{
			# lines that we don't care
		}
	}

	# print statictics information
	my ($defs, $kids);
	for (keys %typeinfo_stub)
	{
		++$defs;
		$kids += defined $typeinfo_stub{$_}{"kids"}? scalar @{$typeinfo_stub{$_}{"kids"}} : 0;
	}
}

#
# Load type information from imtermediate file
#
sub adjust_name($)
{
	# adjust the name so that it works with C/C++, the list is not completed.
	my $name = shift;
	$name =~ s/^(namespace|max|min|operator|template)$/${1}_/;
	return $name;
}

sub load_type_info($)
{
	my $foo = shift;
	open my $fh, "<$foo" or die "$!";

	my %tmp;
	my $cur_obj = undef;
	my $catalog = undef;
	for (<$fh>)
	{
		chomp;
		next if /^#+$|^\s*$/ or -f $_ ;	# ignore commen, empty line and file name
		if (/^Name: (Managed Object|Data Object|Enum|Fault) \- (\w+)/)
		{
			$catalog = $1;
			$cur_obj = $2;
			next;
		}
		elsif (/^\[END\]$/)
		{
			next if scalar keys %tmp == 0;
			$typeinfo_html{'managed'}{$cur_obj} = {%tmp} if $catalog =~ /Managed/;
			$typeinfo_html{'dataobject'}{$cur_obj} = {%tmp} if $catalog =~ /Data/;
			$typeinfo_html{'enum'}{$cur_obj} = {%tmp} if $catalog =~ /Enum/;
			$typeinfo_html{'fault'}{$cur_obj} = {%tmp} if $catalog =~ /Fault/;

			$cur_obj = undef;
			$catalog = undef;
			delete $tmp{$_} for keys %tmp;
		}

		next if not defined $cur_obj;
		if (/^(Desc|Extends): (.+)/)
		{
			my $key = $1;
			my $value = $2;
			$key = (($key =~ /Desc/)? "desc" : "super");
			$value = (($value =~ /<NONE>/)? "" : $value);
			$tmp{$key} = $value;
		}
		elsif (/ => (\w+)(\*?) \((.+?)\):\s+(.*)/)
		{
			# memeber definition
			my %member;
			$member{'name'} = adjust_name $1;
			$member{'optional'} = defined $2? 1 : undef;
			$member{'type'} = $3;
			$member{'others'} = $4;
			$member{'req_ver'} = $2 if $4 =~ /(VI|vSphere) API (\d\.\d)$/;
			push @{$tmp{'members'}}, \%member;
		}
		elsif (/^ #(\w+)\((.+?)?\) \-> (.+)/)
		{
			# method definition
			my %method;
			$method{'name'} = adjust_name $1;
			$method{'return'} = $3;
			if (defined $2)
			{
				my @paras = split /,\s*/, $2;
				for (@paras)
				{
					my %para;
					my @pair = split /\s+/;
					$pair[0] =~ s/xsd://;
					$para{'type'} = $pair[0];
					$para{'name'} = $pair[1];
					push @{$method{'paras'}}, \%para;
				}
			}
			push @{$tmp{'methods'}}, \%method;
		}
		else
		{
#			print "ERROR: $_\n" if not /PROPS|METHODS/;
		}
	}
}

#
# generate source files
#
my %browse_depends;
sub get_wrapper_name($)
{
	my $name = shift;
	for (keys %{$typeinfo_html{'managed'}})
	{
		return $_ if lc $_ eq lc $name;
	}
	undef;
}

sub get_req_member_type($$)
{
	my $req = shift;
	my $mem = shift;
	for (@{$typeinfo_stub{$req}{"kids"}})
	{
		my %kid = %{$_};
		next if $kid{'name'} ne $mem;
		return $kid{'type'};
	}
}

sub generate_type($;$)
{
	my $type = shift;
	my $use_vector = shift;

	if ($type eq "None")
	{
		$type = 'void';
	}
	elsif ($type =~ /^ManagedObjectReference(\[\])? to a /)
	{
		$type = $';
	}
	else
	{
		$type =~ s/^xsd://;
		if ($type =~ /string/)
		{
			if (defined $use_vector)
			{
				$type =~ s/string/std::string/;
			}
			else
			{
				$type =~ s/string/char*/;
			}
		}
		$type =~ s/boolean/bool/;
		$type =~ s/dateTime/time_t/;
		$type =~ s/long/__int64/;
	}

	my $is_array = undef;
	if ($type =~ /\[\]$/)
	{
		$is_array = 1;
		$type =~ s/\[\]$//;
	}

	$type = "Localized$type" if $type eq "MethodFault";
	$type = "${prefix}$type" if exists $typeinfo_html{'dataobject'}{$type}
								or exists $typeinfo_html{'enum'}{$type}
								or exists $typeinfo_html{'fault'}{$type};
	if (defined $is_array)
	{
		if (defined $use_vector)
		{
			$type = "std::vector<$type>"
		}
		else
		{
			$type .= "*";
		}
	}

	return $type;
}

sub is_managed_class($)
{
	my $c = shift;
	return exists $typeinfo_html{'managed'}{$c};
}

sub is_dataobject($)
{
	my $c = shift;
	return exists $typeinfo_html{'dataobject'}{$c};
}

sub is_enumobject($)
{
	my $c = shift;
	return exists $typeinfo_html{'enum'}{$c};
}

sub build_para_list($$)
{
	my $method = shift;
	my $req_class = shift;

	my $paras = "";
	for (@{$method->{'paras'}})
	{
		my %kid = %{$_};
		my $is_pointer = undef;

		if (defined $typeinfo_stub{$req_class}
			and defined $typeinfo_stub{$req_class}{'kids'})
		{
			for (@{$typeinfo_stub{$req_class}{'kids'}})
			{
				my %tmp = %{$_};
				if ($tmp{'name'} eq $kid{'name'})
				{
					next if $tmp{'type'} =~ /soap/
							|| is_managed_class $kid{'type'};
					$is_pointer = 1 if $tmp{'type'} =~ /(\*+)$/
									   and $tmp{'type'} !~ /char\s*\*/;
				}
			}
		}

		$paras .= ", " if $paras ne "";
		my $type = generate_type($kid{type});
		my $para_name = $kid{name};

		if ($kid{type} =~ /\[\]/)
		{
			$paras .= "int size$para_name, ";
		}
		my $raw_type = $type;	# special process for wrapper classes, only one direct is enough here
		$raw_type =~ s/\*$//;

		$paras .= $type;
		$paras .= "* " if defined $is_pointer
						  and $type !~ /int(\d+)?\*/
						  and not is_managed_class $raw_type;
		$paras .= " $para_name";

		$is_pointer = undef;
	}
	return $paras;
}

sub print_header_begin($$)
{
	my $fh = shift;
	my $old = select $fh;

	my $header = shift;
	my $macro = $header;
	$macro =~ s/\./_/;
	print <<DOC;
#ifndef \U$macro\E
#define \U$macro\E

#include <string>
#include <vector>
#include "soapStub.h"				// gSOAP generated
#include "soapVimBindingProxy.h"	// gSOAP generated

namespace vim
{
// forward declarations
typedef xsd__anyType anyType;

// Generic helper to retrieve property of managed object
class mor_handle;
std::vector<${prefix}ObjectContent> get_property(const char* prop, const mor_handle* mo);

// Class definitions
class binding_proxy
{
	binding_proxy(const binding_proxy&);
	binding_proxy& operator=(const binding_proxy&);

	VimBindingProxy* _binding_proxy;
	std::string _url;
public:
	binding_proxy(const char* ip, bool use_ssl);
	~binding_proxy();
	operator VimBindingProxy*() { return _binding_proxy; }
};

class mor_handle
{
protected:
	std::string	 _last_error;
	${prefix}ManagedObjectReference* _mor;

public:
	explicit mor_handle(${prefix}ManagedObjectReference* mor);
	virtual ~mor_handle();

	void set_last_error(const char* msg = 0) { _last_error = msg? msg : ""; }
	std::string get_last_error() const { return _last_error; }

	bool is_type_of(const char* type) const { return _strcmpi(_mor->type, type) == 0; }
	const char* present() const { return _mor->__item; }
	operator ${prefix}ManagedObjectReference*() const { return _mor; }
	operator ${prefix}ManagedObjectReference*() { return _mor; }
	operator bool() const { return _mor != 0; }
};

DOC
	select $old;
}

sub print_header_end($$)
{
	my $fh = shift;
	my $old = select $fh;

	my $header = shift;
	my $macro = $header;
	$macro =~ s/\./_/;
	print <<DOC;

class Vim
{
	binding_proxy* _proxy;
	${prefix}ManagedObjectReference _mor;
	ServiceInstance* _si;
	Vim(binding_proxy* proxy = 0) : _proxy(proxy) {}
	friend Vim& VimInstance(binding_proxy* proxy);

public:
	ServiceInstance& get_service_instance()
	{
		static char type[] =  "ServiceInstance";
		static char item[] =  "ServiceInstance";
		if (!_si)
		{
			_mor.type = type;
			_mor.__item = item;
			_si = new ServiceInstance(&_mor);
		}
		return *_si;
	}
	binding_proxy* proxy() { return _proxy; }
};

Vim& VimInstance(binding_proxy* proxy = 0);
PropertyCollector& get_collector();
} // namespace vim
#endif // \U$macro\E

DOC
	select $old;
}

sub print_source_begin($$)
{
	my $fh = shift;
	my $old = select $fh;
	my $header = shift;

	print <<DOC;
#include <sstream>
#include <cassert>
#include "soapVimBindingProxy.h"
#include "VimBinding.nsmap"
#include "$header"

namespace vim
{
binding_proxy::binding_proxy(const char* ip, bool use_ssl) : _binding_proxy(new VimBindingProxy)
{
	std::stringstream ss;
	ss << (use_ssl? std::string("https") : std::string("http"))
		<< "://" << ip << "/sdk" << std::ends;
	_url = ss.str();
	_binding_proxy->soap_endpoint = _url.c_str();
	if (use_ssl)
	{
		soap_ssl_init();
		if (soap_ssl_client_context(_binding_proxy, SOAP_SSL_NO_AUTHENTICATION,
			NULL, NULL, NULL, NULL, NULL))
		{
			std::ostringstream oss;
			soap_stream_fault(_binding_proxy, oss);
			throw oss.str().c_str();
		}
	}
}

binding_proxy::~binding_proxy()
{
	delete _binding_proxy;
}

mor_handle::mor_handle(${prefix}ManagedObjectReference* mor) : _mor(mor) {}
mor_handle::~mor_handle() {}

template<typename REQ, typename RSP, typename OBJ, typename PROXY>
bool proxy_call_defs(REQ& req, RSP& rsp, OBJ& obj, int (PROXY::*pfm)(REQ*, RSP*))
{
	VimBindingProxy* p = VimInstance().proxy()->operator VimBindingProxy*();
	if ((p->*pfm)(&req, &rsp) == SOAP_OK)
	{
		obj.set_last_error();
		return true;
	}
	else
	{
		obj.set_last_error(p->soap_fault_detail());
		return false;
	}
}
DOC
	select $old;
}

sub print_source_end($)
{
	my $fh = shift;
	my $old = select $fh;
	print <<DOC;

// The entry of ServiceInstance singleton
Vim& VimInstance(binding_proxy* proxy)
{
	static Vim v;
	if (proxy)
		v._proxy = proxy;
	return v;
}

PropertyCollector& get_collector()
{
	ServiceInstance& si = VimInstance().get_service_instance();
	${prefix}ServiceContent sc = si.RetrieveServiceContent();
	static PropertyCollector pc(sc.propertyCollector);
	return pc;
}

std::vector<${prefix}ObjectContent> get_property(const char* prop, const mor_handle* mo)
{
	// ObjectSpec specifies the starting object
	${prefix}ObjectSpec os;
	os.obj = *mo;
	${prefix}ObjectSpec* oss[] = {&os};

	// PropertySpec specifies what properties to retrieve and from type of Managed Object
	char* pps[1]; pps[0] = const_cast<char*>(prop);
	${prefix}PropertySpec ps;
	ps.type = os.obj->type;
	ps.__sizepathSet = 1;
	ps.pathSet = pps;
	${prefix}PropertySpec* pss[] = {&ps};

	// PropertyFilterSpec is used to hold the ObjectSpec and PropertySpec for the call_defs
	${prefix}PropertyFilterSpec pfs;
	pfs.__sizeobjectSet = 1;
	pfs.objectSet = oss;
	pfs.__sizepropSet = 1;
	pfs.propSet = pss;
	${prefix}PropertyFilterSpec* pfSpec[] = {&pfs};

	vim::PropertyCollector pc = get_collector();
	return pc.RetrieveProperties(1, pfSpec);
}
} // namespace vim

DOC
	select $old;
}

sub print_header_forward_decls($)
{
	my $fh = shift;
	my $old = select $fh;
	print "class $_;\n" for sort keys %{$typeinfo_html{'managed'}};
	print "\n";
	select $old;
}

sub print_header_class_begin($$)
{
	my $fh = shift;
	my $old = select $fh;

	my $class = shift;
	my $base = ($typeinfo_html{'managed'}{$class}{'super'} !~ /^\s*$/)?
		$typeinfo_html{'managed'}{$class}{'super'} : "mor_handle";
	print <<DOC;

class $class : public $base
{
public:
	$class(${prefix}ManagedObjectReference* mor = 0);
	virtual ~$class();

	static const char* type;
DOC
	select $old;
}

sub print_header_class_end($)
{
	my $fh = shift;
	my $old = select $fh;
		print <<DOC;
};
DOC
	select $old;
}

sub print_source_class_begin($$)
{
	my $fh = shift;
	my $old = select $fh;

	my $class = shift;
	my $base = ($typeinfo_html{'managed'}{$class}{'super'} !~ /^$/)?
		$typeinfo_html{'managed'}{$class}{'super'} : "mor_handle";
	print <<DOC;

const char* ${class}::type = "$class";
${class}::$class(${prefix}ManagedObjectReference* mor) : $base(mor) {}
${class}::~$class() {}
DOC
	select $old;
}

sub print_header_mem_fun($$$$)
{
	my $fh = shift;
	my $old = select $fh;

	my $return_type = shift;
	my $name = shift;
	my $para_list = shift;
	print <<DOC;
	$return_type $name($para_list);
DOC
	select $old;
}
sub print_header_mem_vars_begin ($)
{
	my $fh = shift;
	my $old = select $fh;
	print <<DOC;

public:
	// Property getter
DOC
	select $old;
}

sub print_header_mem_vars_acc($$$)
{
	my $fh = shift;
	my $old = select $fh;

	my $type = shift;
	my $prop = shift;
	print <<DOC;
	$type get_$prop() const;
DOC
	select $old;
}

sub print_source_mem_vars_acc($$$$)
{
	my $fh = shift;
	my $old = select $fh;

	my $class = shift;
	my $type = shift;
	my $prop = shift;

	my $org_type = $type;
	$org_type =~ s/\bbool\b/xsd__boolean/;
	$org_type =~ s/\bint\b/xsd__int/;
	$org_type =~ s/\bstd::string\b/xsd__string/;
	$org_type =~ s/\bdateTime\b/xsd__dateTime/;
	$org_type =~ s/\bbyte\b/xsd__byte/;
	$org_type =~ s/\btime_t\b/xsd__dateTime/;

	print <<DOC;
$type ${class}::get_$prop() const
{
	std::vector<${prefix}ObjectContent> ocs = get_property("$prop", this);
	if (ocs.size() != 1)
		throw get_last_error();
	if (ocs[0].__sizepropSet == 0) // empty result, not set yet
	{
//		assert(ocs[0].__sizemissingSet == 1);
//		ns1__MissingProperty* miss = ocs[0].missingSet[0];
//		if (miss->fault->fault)
//			throw get_fault_msg(miss->fault->fault);
//		else
			return $type();
	}

	${prefix}DynamicProperty* it = ocs[0].propSet[0];
DOC
	my $ret = undef;
	if ($org_type =~ /vector<(.+?)>/){
		my $t = $1;
		my $val = undef;
		my $arr_type = undef;
		my $append_ = "";
		if (is_managed_class $t)
		{
			$val = "$t(amo->ManagedObjectReference[i])";
			$arr_type = "ManagedObjectReference";
		}
		else
		{
			if ($t =~ /(${prefix}|xsd__)(\w+)/)
			{
				$arr_type = "$2";
			}

			$append_ = '_' if $arr_type =~ /int|long|short/;
			if ($t =~ /string|int|long|short|byte/)
			{
				$val = "amo->${arr_type}${append_}[i]";
			}
			else
			{
				$val = "*amo->${arr_type}[i]";
			}
		}

		print <<DOC;
	${prefix}ArrayOf\u$arr_type* amo = dynamic_cast<${prefix}ArrayOf\u$arr_type*>(it->val);
	$type ret;
	for (int i = 0; i < amo->__size$arr_type$append_; ++i)
		ret.push_back($val);
DOC
		$ret = "ret";
	}
	else
	{
		if ($org_type =~ /xsd__(boolean|int|string|dateTime|byte)/)
		{
			$ret = "dynamic_cast<$org_type*>(it->val)->__item";
		}
		elsif($org_type =~ /${prefix}(.+)/ and exists $typeinfo_html{'enum'}{$1})
		{
			$ret = "dynamic_cast<$prefix${1}_*>(it->val)->__item";
		}
		elsif (is_managed_class $org_type)
		{
			$ret = "$org_type(dynamic_cast<${prefix}ManagedObjectReference *>(it->val))";
		}
		else
		{
			$ret = "*dynamic_cast<$org_type*>(it->val)";
		}
	}
	print <<DOC;
	return $ret;
}

DOC
	select $old;
}

sub print_source_mem_fun_begin($$$$$)
{
	my $fh = shift;
	my $old = select $fh;

	my $class = shift;
	my $return_type = shift;
	my $name = shift;
	my $para_list = shift;

	my $req = $name;
	$req =~ s/_Task$//;

	print <<DOC;

$return_type ${class}::$name($para_list)
{
	${prefix}${req}RequestType req;
	req._USCOREthis = _mor;
DOC
	select $old;
}

sub print_source_mem_fun_req_init($$)
{
	my $fh = shift;
	my $old = select $fh;

	my $req = shift;
	my $prefix_p = undef;
	for (@{$typeinfo_stub{$req}{"kids"}})
	{
		my %kid = %{$_};
		next if $kid{'name'} =~ /_USCOREthis/;

		if ($kid{name}=~ /^__size(.+)/)
		{
			my $name = $1;
				print <<DOC;
	req.$kid{name} = size$name;
DOC
			my $type = get_req_member_type($req, $name);
			$type =~ s/ \*+//;
			if ($type eq "${prefix}ManagedObjectReference")
			{
				print <<DOC;
	$type ** p$name = new $type *[size$name];
	for (int i = 0; i < size$name; ++i)
		p$name\[i\] = $name\[i\];
DOC
				$prefix_p = 1;
			}
		}
		else
		{
			if ($prefix_p)
			{
				print <<DOC;
	req.$kid{name} = p$kid{name};
DOC
				$prefix_p = undef;
			}
			else
			{
				print <<DOC;
	req.$kid{name} = $kid{name};
DOC
			}
		}
	}
	select $old;
}

sub print_source_mem_fun_call_defs($$$)
{
	my $fh = shift;
	my $old = select $fh;

	my $func = shift;
	my $rsp_class = shift;
	my $real_call_defs_name = $func;
	$real_call_defs_name = $`.'_USCORETask' if ($func =~ /_Task$/);
	print <<DOC;

	// call_defs $real_call_defs_name to post the request to ESX server or virtual center
	$rsp_class rsp;
	if (proxy_call_defs(req, rsp, *this, &VimBindingProxy::$real_call_defs_name))
	{
DOC
	select $old;
}

sub print_source_mem_fun_handle_result_void($)
{
	my $fh = shift;
	my $old = select $fh;
	print <<DOC;
		// empty return value
		return;
DOC
	select $old;
}

sub print_source_mem_fun_handle_result_vector($$)
{
	my $fh = shift;
	my $old = select $fh;

	my $ret_type = shift;
	my $no_ns = $ret_type;
	$no_ns =~ s/std:://;
	my $tmp_c = undef;
	if (is_managed_class $ret_type or $ret_type =~ /\bstring\b|\bint\b|\bbool\b/)
	{
		$tmp_c = "rsp.returnval[i]";
	}
	else
	{
		$tmp_c = "*rsp.returnval[i]"
	}
	print <<DOC;
		std::vector<$ret_type> \L${no_ns}s\E;
		for (int i = 0; i < rsp.__sizereturnval; ++i)
		{
			$ret_type tmp($tmp_c);
			\L${no_ns}s\E.push_back(tmp);
		}
		return \L${no_ns}s\E;
DOC
	select $old;
}

sub print_source_mem_fun_handle_result_scalar($$)
{
	my $fh = shift;
	my $old = select $fh;

	my $return_class = shift;
	if (is_managed_class $return_class)
	{
		print "\t\treturn $return_class(rsp.returnval);\n";
	}
	elsif (defined $typeinfo_stub{$return_class})
	{
		print "\t\treturn *rsp.returnval;\n";
	}
	else
	{
		print "\t\treturn static_cast<$return_class>(rsp.returnval);\n";
	}
	select $old;
}

sub print_source_mem_fun_handle_result($$)
{
	my $fh = shift;
	my $return_class = shift;
	if ($return_class =~ /void/)
	{
		print_source_mem_fun_handle_result_void $fh;
	}
	elsif ($return_class =~ /std::vector<(.+?)>/)
	{
		print_source_mem_fun_handle_result_vector $fh, $1;
	}
	else
	{
		print_source_mem_fun_handle_result_scalar $fh, $return_class;
	}
}

sub print_source_mem_fun_end($)
{
	my $fh = shift;
	my $old = select $fh;
	print <<DOC;
	}

	throw get_last_error();
}
DOC
	select $old;
}

sub print_browser_header_begin($$$)
{
	my $fh = shift;
	my $header = shift;
	my $wrapper_hedaer = shift;
	my $old = select $fh;

	my $macro = $header;
	$macro =~ s/\./_/;
	print <<DOC;
#ifndef \U$macro\E
#define \U$macro\E

#include <iostream>
#include <cassert>
#include "$wrapper_hedaer"

namespace vim_browser
{
using namespace vim;
//
// == The simple browsing protocol ==
// Browse will send following message to receiver in order:
// 1. pair("Begin", class_name). indicate start the browsing
// 2. pair("Name", member_name), pair("Type", member_type) and
//    pair("Value", member_value)
//    if the value is an array, it's seperated by '|', started with '\@'
// 3. optional pair("Base", base_class_name), report the base member to be browsed,
//    followed by a list of base members, just as item 2 format
// 4. pair("End", ""), indicate end the browsing
//
typedef void (*pf_update)(const std::string& key, const std::string& value, void* context);

// Interface for browser. All specific browsable types should derive from it
struct closure
{
	virtual ~closure() {}
	virtual closure* get_sub_closure(size_t index) const = 0;
	virtual void browse(pf_update update, void* context) const = 0;
	virtual const char* type() const = 0;
};
DOC
	select $old;
}

sub print_browser_header_end($$)
{
	my $fh = shift;
	my $header = shift;
	my $old = select $fh;

	my $macro = $header;
	$macro =~ s/\./_/;
	print <<DOC;

// browse handler for different types
//template<typename T> std::string handle_obj(T t);
DOC
	for (sort keys %browse_depends)
	{
		my $type = $_;
		if (not is_enumobject $type)
		{
			if (is_managed_class $type)
			{
				print <<DOC;

inline std::string handle_obj($type obj)
	{ return obj? obj.present() : "_unset_"; }
DOC
			}
			else
			{
				$type = "$prefix$type" if exists $typeinfo_html{'dataobject'}{$type} or
					exists $typeinfo_html{'fault'}{$type};
				my $present = "\"<$type>\"";
				$present = "obj.__item" if $type =~ /ManagedObjectReference/;
				print <<DOC;

inline std::string handle_obj($type obj)
	{ return $present; }
DOC
			}
		}
	}

	for (sort keys %browse_depends)
	{
		my $type = $_;
		if (exists $typeinfo_html{'enum'}{$type})
		{
			print <<DOC;

inline std::string handle_obj($prefix$type obj)
{
	switch (obj)
	{
DOC
			my %enums = %{$typeinfo_stub{$prefix.$type}{"kids"}[0]{'values'}};
			for (keys %enums)
			{
				print <<DOC;
	case $prefix${type}__$enums{$_}:
		return "$enums{$_}";
DOC
			}
			print <<DOC;
	default:
		assert(!"should not be here");
		return "<invalid_enum_value>";
	}
}
DOC
		}
	}

	print <<DOC;

template<typename T>
std::string handle_obj_ptr(const T* value)
{
	return value? handle_obj(*value) : "_unset_";
}

closure* get_closure_from_mor(${prefix}ManagedObjectReference* mor);
} // namespace vim_browser
#endif // \U$macro\E
DOC
	select $old;
}

sub print_browser_source_begin($$)
{
	my $fh = shift;
	my $header = shift;
	my $old = select $fh;
	my $sep = '|';
	my $start = '@';

	print <<DOC;
#include <vector>
#include <sstream>
#include "$header"

namespace vim_browser
{
// convert to string, instace
template <typename T> std::string basic2str(const T& t)
{
	std::stringstream ss;
	ss << std::boolalpha << t;
	return ss.str();
}

// convert to string, pointer
template <typename T> std::string basic2str(T* t)
{
	if (!t)
		return "_unset_";
	std::stringstream ss;
	ss << *t;
	return ss.str();
}

// specialize for xsd_anyType
template <> std::string basic2str(xsd__anyType* t)
{
	if (!t)
		return "_unset_";
	std::stringstream ss;
	ss << std::boolalpha << t->__item;
	return ss.str();
}

// specialize for char*
template <> std::string basic2str(char* t)
{
	if (!t)
		return "_unset_";
	std::stringstream ss;
	ss << t;
	return ss.str();
}

// specialize for bool*
template <> std::string basic2str(bool* t)
{
	if (!t)
		return "_unset_";
	std::stringstream ss;
	ss << std::boolalpha <<  *t;
	return ss.str();
}

// convert to string, C-style array
template <typename T> std::string a2s_basic(const T* t, int size)
{
	if (!t)
		return "_unset_";
	std::stringstream ss;
	ss << '$start';
	for (int i = 0; i < size; ++i)
	{
		if (i != 0)
			ss << '$sep';
		ss << std::boolalpha << t\[i\];
	}
	return ss.str();
}

// convert to string, STL-style array
template <typename T> std::string a2s_basic(const std::vector<T>& t)
{
	std::stringstream ss;
	ss << '$start';
	for (size_t i = 0; i < t.size(); ++i)
	{
		if (i != 0)
			ss << '$sep';
		ss << std::boolalpha << t\[i\];
	}
	return ss.str();
}

// convert to string, C-style object array
template <typename T> std::string a2s_object(const T* t, int size)
{
	if (!t)
		return "_unset_";
	std::stringstream ss;
	ss << '$start';
	for (int i = 0; i < size; ++i)
	{
		if (i != 0)
			ss << '$sep';
		ss << std::boolalpha << handle_obj(*t\[i\]);
	}
	return ss.str();
}

// convert to string, STL-style object array
template <typename T> std::string a2s_object(const std::vector<T>& t)
{
	std::stringstream ss;
	ss << '$start';
	for (size_t i = 0; i < t.size(); ++i)
	{
		if (i != 0)
			ss << '$sep';
		ss << std::boolalpha << handle_obj(t\[i\]);
	}
	return ss.str();
}

// version check routine
extern bool is_higher_version_than(const char* ver);

void update_member(pf_update update, const std::string& name, 
	const std::string& type, const std::string& value, void* context)
{
	update("Name", name, context);
	update("Type", type, context);
	update("Value", value, context);
}

void update_member_with_version(pf_update update, const std::string& name,
	const std::string& type, const std::string& value, void* context, const char* ver)
{
	update("Name", name, context);
	update("Type", type, context);
	if (is_higher_version_than(ver))
		update("Value", value, context);
	else
		update("Value", "_unsupported_", context);
}
DOC
	select $old;
}

sub print_browser_source_end($)
{
	my $fh = shift;
	my $old = select $fh;

	print <<DOC;

closure* get_closure_from_mor(${prefix}ManagedObjectReference* mor)
{
	if (0)
		;
DOC

	for (sort keys %{$typeinfo_html{'managed'}})
{
		my $name = $_;
		print <<DOC;
	else if (strcmp(mor->type, "$name") == 0)
		return new ${name}_closure($name(mor));
DOC
	}
	
	print <<DOC;
	else
		return 0;
}
} // namespace vim_browser
DOC
	select $old;
}

sub print_browser_header($$;$)
{
	my $fh = shift;
	my $old = select $fh;

	my $type = shift;
	my $select_key = shift;
	my $p = defined $select_key? "$prefix" : "";
	print <<DOC;

class ${type}_closure : public closure
{
protected:
	$p$type _obj;
public:
	${type}_closure(const $p$type& obj) : _obj(obj) {}
	virtual ~${type}_closure() {}
	virtual closure* get_sub_closure(size_t index) const;
	virtual void browse(pf_update update, void* context) const;
	virtual const char* type() const { return "$type"; }
};
DOC
	select $old;
}

sub print_browser_source($$;$)
{
	my $fh = shift;
	my $old = select $fh;

	my $type = shift;
	my $select_key = shift;
	my $p = defined $select_key? "$prefix" : "";
	print <<DOC;

void ${type}_closure::browse(pf_update update, void* context) const
{
	update("Begin", "$type", context);
DOC

	my $member_aref = defined $select_key?
		$typeinfo_html{$select_key}{$type}{'members'} : $typeinfo_html{'managed'}{$type}{'members'};
	my $parent = defined  $select_key?
		$typeinfo_html{$select_key}{$type}{'super'} : $typeinfo_html{'managed'}{$type}{'super'};

	# members
	my @browsable_members;
	if (defined $member_aref)
	{
		my $is_array = undef;
		my @props = @{$member_aref};
		my $index = 0;
		for (@props)
		{
			my %kid = %{$_};
			my $sub_type = $kid{'type'};
			my $name = $kid{'name'};

			my $exp = undef;
			my $value = defined $select_key? "_obj.$kid{'name'}" : "_obj.get_$name()";
			if ($sub_type =~ /(.+?)\[\]/)
			{
				my $t = $1;
				my %item;
				$item{'is_array'} = 1;
				if (defined $select_key)
				{
					my $count = "_obj.__size$kid{'name'}";
					if ($t =~ /xsd\:/ || exists $typeinfo_html{'enum'}{$t})
					{
						if ($t =~ /string/)
						{
							$exp = "a2s_basic(*$value, $count)";
						}
						else
						{
							$exp = "a2s_basic($value, $count)";
						}
					}
					else
					{
						$item{value} = $value;
						$browse_depends{$t} = 1;
						$exp = "a2s_object($value, $count)";
					}
				}
				else
				{
					if ($t =~ /xsd\:/ || exists $typeinfo_html{'enum'}{$t})
					{
						$exp = "a2s_basic($value)";
					}
					else
					{
						$item{value} = $value;
						$browse_depends{$t} = 1;
						$exp = "a2s_object($value)";
					}
				}
				$browsable_members[$index] = \%item if exists $item{'value'};
			}
			else
			{
				# get whether this member is * qualified
				my $is_pointer = undef;
				my $done = undef;
				for (@{$typeinfo_stub{$p.$type}{'kids'}})
				{
					my %this = %{$_};
					for (keys %this)
					{
						if ($this{'name'} eq $kid{'name'})
						{
							$is_pointer = 1 if $this{'type'} =~ /\*/;
							$done = 1;
							last;
						}
					}
					last if defined $done;
				}

				my %item;
				if ($sub_type =~ /xsd\:/)
				{
					if (defined $is_pointer)
					{
#						$exp = "($value? basic2str($value) : \"_unset_\")";
						$exp = "basic2str($value)";
					}
					else
					{
						$exp = "$value";
						$exp .= ".__item" if $sub_type =~ /anyType/;	### TODO special case
						$exp = "basic2str($exp)";
					}
					if ($type =~ /ManagedObjectReference/ and $kid{'name'} eq "value")
					{
						$exp = "_obj.__item";							### TODO special case
					}
				}
				else
				{
					$browse_depends{$sub_type} = 1;
					if (defined $is_pointer)
					{
						$exp = "handle_obj_ptr($value)";
					}
					else
					{
						$exp = "handle_obj($value)";
					}
					$item{'is_pointer'} = $is_pointer;
					$item{'value'} = $value if not is_enumobject $sub_type;
				}
				$browsable_members[$index] = \%item if exists $item{'value'};
			}

			if (exists $kid{'req_ver'})
			{
			print <<DOC;
	update_member_with_version(update, "$name", "$sub_type", $exp, context, "$kid{'req_ver'}");
DOC
			}
			else
			{
				print <<DOC;
	update_member(update, "$name", "$sub_type", $exp, context);
DOC
			}
			++$index;
		}
	}

	# the derved members
	if ($parent !~ /^$/)
	{
		print <<DOC;
	update("Base", "$parent", context);
	${parent}_closure(dynamic_cast<const $p$parent&>(_obj)).browse(update, context);
DOC
	}

	print <<DOC;
	update("End", std::string(), context);
}

DOC
	print <<DOC;
closure* ${type}_closure::get_sub_closure(size_t index) const
{
DOC

	if (is_managed_class $type)
	{
		print <<DOC;
	if (!_obj)
		return 0;

DOC
	}

	if (scalar @browsable_members > 0)
	{
		print <<DOC;
	int prop_index = index >> 16;
	switch (prop_index - 1)	// property is 1 based!
	{
DOC
		if (defined $member_aref)
		{
			my @props = @{$member_aref};
			my $index = 0;
			for (@props)
			{
				my %kid = %{$_};
				my $sub_type = $kid{'type'};
				if (defined $browsable_members[$index])
				{
					if (exists $browsable_members[$index]{'is_pointer'}
						and not is_managed_class $type)
					{
						if (is_managed_class $sub_type)
						{
							print <<DOC;
	case $index:
		return $browsable_members[$index]{'value'}? 
			get_closure_from_mor($sub_type($browsable_members[$index]{'value'})) : 0;
DOC
						}
						else
						{
							print <<DOC;
	case $index:
		if ($browsable_members[$index]{'value'})
			return new ${sub_type}_closure(*$browsable_members[$index]{'value'});
		else
			return 0;
DOC
						}
					}
					elsif (exists $browsable_members[$index]{'is_array'})
					{
						$sub_type =~ s/\[\]$//;
						if (is_managed_class $type)
						{
							$sub_type = "View" if $sub_type =~ /ManagedObjectReference/; # TODO special case
							my $sub_p = is_managed_class $sub_type? "" : "$prefix";
							my $ret = is_managed_class $sub_type?
								"get_closure_from_mor(v\[i\])" :
								"new ${sub_type}_closure(v\[i\])";
							print <<DOC;
	case $index:
		{
			int arr_index = index & 0xFFFF;
			size_t i = --arr_index;	// array index is 1 based also
			std::vector<$sub_p${sub_type}> v = $browsable_members[$index]{'value'};
			if (i < v.size())
				return $ret;
			else
				return 0;
		}
DOC
						}
						else
						{
							my $star = is_managed_class $sub_type? "" : "*";
							print <<DOC;
	case $index:
		{
			int i = ((index&0xFFFF)>>16);
			if (i < _obj.__size$kid{'name'})
				return new ${sub_type}_closure($star$browsable_members[$index]{'value'}\[i\]);
			else
				return 0;
		}
DOC
						}
					}
					else
					{
						print <<DOC;
	case $index:
		return new ${sub_type}_closure($browsable_members[$index]{'value'});
DOC
					}
				}
				++$index;
			}
		}
		
		if ($parent !~ /^$/)
		{
			my $mem_count = scalar @{$member_aref};
			print <<DOC;
	default:
		int arr_index = index & 0xFFFF;
		return ${parent}_closure(_obj).get_sub_closure(((prop_index - $mem_count) << 16) + arr_index);
	}
DOC
		}
		else
		{
			print <<DOC;
	default:
		return 0;
	}
DOC
		}
	}
	print <<DOC;
	return 0;
}
DOC
	select $old;
}

sub generate_wrapper($$)
{
	my ($header, $typeinfo_stub) = @_;
	open my $hf, ">$header" or die "$!";
	open my $cf, ">$typeinfo_stub" or die "$!";

	my ($file, $dir) = fileparse $header;

	print_header_begin $hf, $file;
	print_source_begin $cf, $file;
	print_header_forward_decls $hf;

	# Sort the class: base class > derived class
	my @left = sort keys %{$typeinfo_html{'managed'}};
	my @ordered;
	my %processed;
	while (1)
	{
		my @tmp;
		for (@left)
		{
			if ($typeinfo_html{'managed'}{$_}{'super'} =~ /^$/
				or exists $processed{$typeinfo_html{'managed'}{$_}{'super'}})
			{
				push @ordered, $_;
				$processed{$_} = 1;
			}
			else
			{
				push @tmp, $_;
			}
		}
		@left = @tmp;
		last if scalar @left == 0;
	}

	# Implement each class
	for my $class (@ordered)
	{
		print_header_class_begin $hf, $class;
		print_source_class_begin $cf, $class;

		# methods
		for my $method (@{$typeinfo_html{'managed'}{$class}{'methods'}})
		{
			my $func_name = $method->{'name'};
			my $req_class_name = $func_name =~ /_Task$/?
				$name_to_C_type{$`."RequestType"} :
				$name_to_C_type{$func_name."RequestType"};

			my $return = generate_type($method->{'return'}, 1);
			my $paras = build_para_list $method, $req_class_name;
			print_header_mem_fun $hf, $return, $func_name, $paras;
			print_source_mem_fun_begin $cf, $class, $return, $func_name, $paras;
			print_source_mem_fun_req_init $cf, $req_class_name;

			my $rsp_class_name = $name_to_C_type{$func_name.'Response'};
			print_source_mem_fun_call_defs $cf, $func_name, $rsp_class_name;

			print_source_mem_fun_handle_result $cf, $return;
			print_source_mem_fun_end $cf;
		}

		# properties
		print_header_mem_vars_begin $hf;
		my $arr_ref = $typeinfo_html{'managed'}{$class}{'members'};
		if (defined $arr_ref)
		{
			for my $prop (@{$arr_ref})
			{
				my %member = %{$prop};
				my $type = generate_type $member{type}, 1;
				if ($type =~ /${prefix}ManagedObjectReference/)
				{
					# replace managed object with wrapper object
					my $wrapper = get_wrapper_name $member{name};
					$type =~ s/${prefix}ManagedObjectReference/$wrapper/ if defined $wrapper;
				}

				print_header_mem_vars_acc $hf, $type, $member{name};
				print_source_mem_vars_acc $cf, $class, $type, $member{name};
			}
		}

		print_header_class_end $hf;
	}

	print_header_end $hf, $file;
	print_source_end $cf;
	
	close $hf;
	close $cf;
}

sub generate_browser($$$)
{
	my ($header, $typeinfo_stub, $wrapper_header) = @_;
	my ($file, $dir) = fileparse $header;

	open my $bh, ">$header" or die "$!";
	open my $bc, ">$typeinfo_stub" or die "$!";
	print_browser_header_begin $bh, $file, $wrapper_header;
	print_browser_source_begin $bc, $file;

	for (sort keys %{$typeinfo_html{'dataobject'}})
	{
		print_browser_header $bh, $_, "dataobject";
		print_browser_source $bc, $_, "dataobject";
	}
	for (sort keys %{$typeinfo_html{'fault'}})
	{
		print_browser_header $bh, $_, "fault";
		print_browser_source $bc, $_, "fault";
	}
	for (sort keys %{$typeinfo_html{'managed'}})
	{
		print_browser_header $bh, $_;
		print_browser_source $bc, $_;
	}
	print_browser_header_end $bh, $file;
	print_browser_source_end $bc;
	close $bh;
	close $bc;
}

#
# Phase 1: generating client stub source files with gSOAP
#
sub generate_stub_with_gsoap($$)
{
	my $wsdl_file = shift;
	my $target = shift;
	die "$wsdl_file is not exist\n" if not -e $wsdl_file;

	my $tmp = ".gsoap_tmp.h";	
	my ($file, $dir) = fileparse $wsdl_file;
	my $xsd_files = join " ", <${dir}*.xsd>; # all .xsd files
	
	# Opiont explanation
	#  -s: don't generate STL code, use consr* instead of std:string
	#  -o; output file
	#  -n: the namespace prefix
	my $wsdl2h = "wsdl2h.exe -s -o$tmp -n$ns $wsdl_file $xsd_files > NUL 2>&1";
	system $wsdl2h;
	die "call wsdl2h failed: $!\n" if $? == -1;
	return undef if ($? >> 8) != 0;
	
	# Opiont explanation
	#  -x: don't generate STL code, need not these
	#  -L: don't generate soapClientLib/soapServerLib, needn not this
	#  -i: generate service proxies and objects inherited from soap struct
	#  -C: generate client-side code only
	my $soapcpp2 = "soapcpp2.exe $tmp -x -L -i -C -d$target > NUL 2>&1";
	system($soapcpp2);
	die "call soapcpp2 failed: $!\n" if $? == -1;
	return undef if ($? >> 8) != 0;
	1;
}

#
# Phase 2: abstract type info from HTML doc file and write to intermediate file
#
sub extract_info_from_help_doc($)
{
	my $doc_path = shift;
	die "$doc_path is not exist" if not -e "$doc_path";

#	my @doc_files = ("$doc_path/vim.FileManager.html");
	my @doc_files = sort {uc($a) cmp uc($b)} <$doc_path/*.html>;
	my $tmp_file = ".doc_typeinfo.txt";

	open my $fh, ">$tmp_file" or die "Cannot open $tmp_file: $!";
	parse_html_doc($_, $fh) for @doc_files;
	close $fh;
}

#
# Phase 3: generating wapper and browser source
#
sub generate($)
{
	my $target = shift;
	load_type_info ".doc_typeinfo.txt";
	parse_stub_header "$target\\soapStub.h";

	generate_wrapper "$target\\vim_wrapper.h", "$target\\vim_wrapper.cpp";
	generate_browser "$target\\vim_browser.h", "$target\\vim_browser.cpp", "vim_wrapper.h";
}

#
# Entry
#
sub usage
{
	print <<DOC;
Usage:
    step1: parse WSDL definition with gSOAP
    ** Make sure gSOAP binary is in \$PATH environment variable **
    -p <wsdl_file> [output]
       wsdl_file: the file path of file vimService.wsdl
	   output: the output directory. use current directory if not specified

    step2: extract type informatio from SDK help docs 
    -e <vim_sdk_help_path>
       sdk_help_folder: the folder path of SDK help file index.html

    step3: generate the wrapper & browser source 
    -g [output]
	   output: the output directory. use current directory if not specified

DOC
	exit 0;
}

sub main
{
	if (not defined $ARGV[0])
	{
		usage;
	}
	elsif ($ARGV[0] eq "-p")
	{
		usage if $#ARGV < 1;
		my $target = (defined $ARGV[1])? $ARGV[1] : '.';
		die "Target folder are not exist: $target\n" if not -d $target;
		print "Generating client stub ... ";
		generate_stub_with_gsoap $ARGV[1], $target;
		print "DONE\n";
		print "Please run again with -e to read type information from help.\n"
	}
	elsif ($ARGV[0] eq "-e")
	{
		usage if $#ARGV < 1;
		print "Retrieve type information from vim SDK help files ... ";
		extract_info_from_help_doc $ARGV[1];
		print "DONE\n";
		print "Please run again with -g to generate SDK C++ Wrapper.\n"
	}
	elsif ($ARGV[0] eq "-g")
	{
		my $target = (defined $ARGV[1])? $ARGV[1] : '.';
		die "Target folder are not exist: $target\n" if not -d $target;
		print "Generating vSphere SDK C++ wrapper ... ";
		generate $target;
		print "DONE\n";
		print "You cam build the codes now.\n";
	}
	else
	{
		usage;
	}
}
main;

__DATA__
Note:
1. browsing cares the version while get_sub_closure does not. [FIXED]
2. on build, make macro WITH_OPENSSL and WITH_COOKIES defined both
