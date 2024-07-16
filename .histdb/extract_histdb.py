import time
from functools import partial

import numpy as np
from datasets.utils.logging import set_6



select start_time as time, ses, dir, argv as cmd from (select session, replace(places.dir, '/home/jfa', '~') as dir, replace(commands.argv, '
', '
') as argv
from
  commands
  join history on history.command_id = commands.id
  join places on history.place_id = places.id
where 1
group by history.command_id, history.place_id
order by time asc
