import os.path

import config

from bobb.registrars import builder, action
from bobb.operations import run, cd, abort, log

this_dir = os.path.abspath(__file__)
if not os.path.isdir(this_dir):
  this_dir = os.path.abspath(os.path.join(this_dir, '..'))

@builder(
  dir=this_dir,
  tgts=['handin.tgz'],
  deps=config.handin_files
)
def build_handin_tgz():
  run('tar cvzf handin.tgz %s' % ' '.join(config.handin_files))

@action(
  dir=this_dir,
  deps=['handin.tgz']
)
def package():
  pass

@action(
  dir=this_dir,
  deps=['handin.tgz']
)
def submit():
  user = run('whoami')
  assessment = config.lab_name
  destination = run('wget -q -O- "http://unofficial.fish.ics.cs.cmu.edu/officialSubmit.rb?' +
                    'course=15150-f13&user=%s&assessment=%s"' % (user, assessment))
  run('cp handin.tgz %s' % destination)
  out = run('wget -q -O- "http://unofficial.fish.ics.cs.cmu.edu/officialSubmit.rb?' +
      ('course=15150-f13&user=%s&assessment=%s&submit=handin.tgz" | ' % (user, assessment)) +
       'grep "Submission received"', check_retval=False)

  if out.return_code == 0:
    log('Submission received. Please verify your score on Autolab.')
  else:
    abort('Submission NOT received. Contact a TA.')

@action(
  dir=this_dir,
  deps=['sources.cm']
)
def compile():
  runner = \
    ('val _ =\n'
     '  if CM.make "sources.cm"\n'
     '  then OS.Process.exit(OS.Process.success)\n'
     '  else OS.Process.exit(OS.Process.failure)')

  f = open('compile.sml', 'w')
  f.write(runner)
  f.close()

  out = run('sml -m compile.sml', check_retval=False)
  run('rm compile.sml')

  if out.return_code != 0:
    abort('Your code does not compile cleanly!')
