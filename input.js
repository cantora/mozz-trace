var mozz_input = { 
  'name': 'asdf',
  'base': 0x08048000,
  'instructions': [
    { 'dasm':  'int 3', 
      'bytes': [0xcc]
    },
    { 'dasm':  'movl $5, %eax',
      'bytes': [0xb8, 0x05, 0x00, 0x00, 0x00]
    },
    { 'dasm':  'movl %eax, %ebx',
      'bytes': [0x89, 0xc3]
    },
    { 'dasm':  'leave',
      'bytes': [0xc9]
    },
    { 'dasm':  'subl $78, %ebp',
      'bytes': [0x83, 0xed, 0x4e]
    },
    { 'dasm':  'int 3', 
      'bytes': [0xcc]
    },
    { 'dasm':  'leave',
      'bytes': [0xc9]
    },
    { 'dasm':  'movl $5, %eax',
      'bytes': [0xb8, 0x05, 0x00, 0x00, 0x00]
    },
    { 'dasm':  'int 3', 
      'bytes': [0xcc]
    },
    { 'dasm':  'leave',
      'bytes': [0xc9]
    },
    { 'dasm':  'ret',
      'bytes': [0xc3]
    }
  ],
  'path': {
    'type': 'path',
    'repeats': 0,
    'path': [
      { 'idx': 0, 'type': 'instr' },
      { 'idx': 1, 'type': 'instr' },
      { 'idx': 3, 'type': 'instr' },
      { 'type': 'path',
        'repeats': 4,
        'path': [
          { 'idx': 4, 'type': 'instr' },
          { 'idx': 5, 'type': 'instr' },
          { 'idx': 7, 'type': 'instr' }
        ]          
      },
      { 'idx': 4, 'type': 'instr' },
      { 'idx': 6, 'type': 'instr' },
      { 'idx': 8, 'type': 'instr' },
      { 'idx': 9, 'type': 'instr' },
      { 'idx': 10, 'type': 'instr' }
    ]
  },
  'meta': [
    { 'eax': 0, 'ebx': 3 },
    { 'eax': 1, 'ebx': 3 },
    { 'eax': 2, 'ebx': 3 },
    { 'eax': 3, 'ebx': 3 },
    { 'eax': 4, 'ebx': 3 },
    { 'eax': 5, 'ebx': 3 },
    { 'eax': 6, 'ebx': 3 },
    { 'eax': 7, 'ebx': 3 },
    { 'eax': 8, 'ebx': 3 },
    { 'eax': 9, 'ebx': 3 },
    { 'eax': 10, 'ebx': 3 },
    { 'eax': 11, 'ebx': 3 },
    { 'eax': 12, 'ebx': 3 },
    { 'eax': 13, 'ebx': 3 },
    { 'eax': 14, 'ebx': 3 },
    { 'eax': 15, 'ebx': 3 },
    { 'eax': 16, 'ebx': 3 },
    { 'eax': 18, 'ebx': 3 },
    { 'eax': 19, 'ebx': 3 },
    { 'eax': 20, 'ebx': 3 },
    { 'eax': 21, 'ebx': 3 },
    { 'eax': 22, 'ebx': 3 }
  ]
}

