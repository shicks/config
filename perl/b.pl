use Sdh::Util;

my $foo = "a\nc\nd\nb\n";
my $bar = '';
#my $res = Sdh::Util::run(['echo', 'abc'], '$`G`$ ', '<-', '>$`Ki`> ', \$bar, '?^');
#my $res = Sdh::Util::run(['sort'], '$`G`$ ', '<$`b`< ', \$foo, '>$`Ki`> ', \$bar, '?^');
my $res = Sdh::Util::run(['false'], '$`G`$ ', '<-', '>$`Ki`> ', \$bar, '?^');

print "RESULT: $res\nOUT: $bar\n";
