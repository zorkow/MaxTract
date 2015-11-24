#!/usr/bin/python 

import json
import sys

class Compare(object):
  """Compare two json files."""

  def __init__(self, file1, file2):
    
    self.jsons1 = self.LoadJson(file1)
    self.jsons2 = self.LoadJson(file2)

  def LoadJson(self, filename):
    fp = open(filename, 'r')
    js = json.load(fp)
    fp.close()
    return js
  
  def Compare(self):
    for slot in ['ClipImage', 'SrcImage', 'PageHeight', 'PageWidth', 'ClipY',
                 'ClipX', 'ClipHeight', 'Page', 'ClipWidth']:
      if self.jsons1[slot] != self.jsons2[slot]:
        return False
    glyphs1 = self.jsons1['glyphs']
    glyphs2 = self.jsons2['glyphs']

    return set([repr(x) for x in glyphs1]) == set([repr(x) for x in glyphs2])

if __name__ == '__main__':
  if len(sys.argv) != 3:
    print "Usage: %s file1.json file2.json" % sys.argv[0]
    sys.exit(1)
  app = Compare(sys.argv[1], sys.argv[2])
  if app.Compare():
    print "Correct"
  else:
    print "Not Correct!"
    
