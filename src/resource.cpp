/* Raw - Another World Interpreter
 * Copyright (C) 2004 Gregory Montoir
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

#include "resource.h"
#include "bank.h"
#include "file.h"
#include "serializer.h"
#include "video.h"
#include "util.h"
#include "parts.h"

#include <vector>
#include <string>
#include <iostream>
#include <SDL.h>


uint8_t  framebuffers[4*64000];
int      framebuffer_ids[4] = {-1,-1,-1,-1};
int      numstored_framebuffers = 0;

// #define USE_DUMP

Resource::Resource(Video *vid, const char *dataDir)
	: video(vid), _dataDir(dataDir), currentPartId(0),requestedNextPart(0) {
}

void Resource::readBank(const MemEntry *me, uint8_t *dstBuf) {
#ifndef USE_DUMP
	uint16_t n = me - _memList;
	debug(DBG_BANK, "Resource::readBank(%d)", n);

	Bank bk(_dataDir);
	if (!bk.read(me, dstBuf)) {
		error("Resource::readBank() unable to unpack entry %d\n", n);
	}
#endif
}

static const char *resTypeToString(unsigned int type)
{
	static const char* resTypes[]=
	{
		"RT_SOUND",
		"RT_MUSIC",
		"RT_POLY_ANIM",
		"RT_PALETTE",
		"RT_BYTECODE",
		"RT_POLY_CINEMATIC"
	};
	if (type >= (sizeof(resTypes) / sizeof(const char *)))
		return "RT_UNKNOWN";
	return resTypes[type];
}

#define RES_SIZE 0
#define RES_COMPRESSED 1
int resourceSizeStats[7][2];
#define STATS_TOTAL_SIZE 6
int resourceUnitStats[7][2];

/*
	Read all entries from memlist.bin. Do not load anything in memory,
	this is just a fast way to access the data later based on their id.
*/
void Resource::readEntries() {
#ifndef USE_DUMP
	File f;
	int resourceCounter = 0;


	if (!f.open("memlist.bin", _dataDir)) {
		error("Resource::readEntries() unable to open 'memlist.bin' file\n");
		//Error will exit() no need to return or do anything else.
	}

	//Prepare stats array
	memset(resourceSizeStats,0,sizeof(resourceSizeStats));
	memset(resourceUnitStats,0,sizeof(resourceUnitStats));

	_numMemList = 0;
	MemEntry *memEntry = _memList;
	while (1) {
		assert(_numMemList < ARRAYSIZE(_memList));
		memEntry->state = f.readByte();
		memEntry->type = f.readByte();
		memEntry->bufPtr = 0; f.readUint16BE();
		memEntry->unk4 = f.readUint16BE();
		memEntry->rankNum = f.readByte();
		memEntry->bankId = f.readByte();
		memEntry->bankOffset = f.readUint32BE();
		memEntry->unkC = f.readUint16BE();
		memEntry->packedSize = f.readUint16BE();
		memEntry->unk10 = f.readUint16BE();
		memEntry->size = f.readUint16BE();

    if (memEntry->state == MEMENTRY_STATE_END_OF_MEMLIST) {
      break;
    }



		//Memory tracking
		if (memEntry->packedSize==memEntry->size)
		{
			resourceUnitStats[memEntry->type][RES_SIZE] ++;
			resourceUnitStats[STATS_TOTAL_SIZE][RES_SIZE] ++;
		}
		else
		{
			resourceUnitStats[memEntry->type][RES_COMPRESSED] ++;
			resourceUnitStats[STATS_TOTAL_SIZE][RES_COMPRESSED] ++;
		}

		resourceSizeStats[memEntry->type][RES_SIZE] += memEntry->size;
		resourceSizeStats[STATS_TOTAL_SIZE][RES_SIZE] += memEntry->size;
		resourceSizeStats[memEntry->type][RES_COMPRESSED] += memEntry->packedSize;
		resourceSizeStats[STATS_TOTAL_SIZE][RES_COMPRESSED] += memEntry->packedSize;

		debug(DBG_RES, "R:0x%02X, %-17s size=%5d (compacted gain=%2.0f%%)",
				resourceCounter,
				resTypeToString(memEntry->type),
				memEntry->size,
				memEntry->size ? (memEntry->size-memEntry->packedSize) / (float)memEntry->size * 100.0f : 0.0f);

		resourceCounter++;

		_numMemList++;
		memEntry++;
	}

	debug(DBG_RES,"\n");
	debug(DBG_RES,"Total # resources: %d",resourceCounter);
	debug(DBG_RES,"Compressed       : %d",resourceUnitStats[STATS_TOTAL_SIZE][RES_COMPRESSED]);
	debug(DBG_RES,"Uncompressed     : %d",resourceUnitStats[STATS_TOTAL_SIZE][RES_SIZE]);
	debug(DBG_RES,"Note: %2.0f%% of resources are compressed.",100*resourceUnitStats[STATS_TOTAL_SIZE][RES_COMPRESSED]/(float)resourceCounter);

	debug(DBG_RES,"\n");
	debug(DBG_RES,"Total size (uncompressed) : %7d bytes.",resourceSizeStats[STATS_TOTAL_SIZE][RES_SIZE]);
	debug(DBG_RES,"Total size (compressed)   : %7d bytes.",resourceSizeStats[STATS_TOTAL_SIZE][RES_COMPRESSED]);
	debug(DBG_RES,"Note: Overall compression gain is : %2.0f%%.",
		(resourceSizeStats[STATS_TOTAL_SIZE][RES_SIZE] - resourceSizeStats[STATS_TOTAL_SIZE][RES_COMPRESSED])/(float)resourceSizeStats[STATS_TOTAL_SIZE][RES_SIZE]*100);

	debug(DBG_RES,"\n");
	for(int i=0 ; i < 6 ; i++)
		debug(DBG_RES,"Total %-17s unpacked size: %7d (%2.0f%% of total unpacked size) packedSize %7d (%2.0f%% of floppy space) gain:(%2.0f%%)",
			resTypeToString(i),
			resourceSizeStats[i][RES_SIZE],
			resourceSizeStats[i][RES_SIZE] / (float)resourceSizeStats[STATS_TOTAL_SIZE][RES_SIZE] * 100.0f,
			resourceSizeStats[i][RES_COMPRESSED],
			resourceSizeStats[i][RES_COMPRESSED] / (float)resourceSizeStats[STATS_TOTAL_SIZE][RES_COMPRESSED] * 100.0f,
			(resourceSizeStats[i][RES_SIZE] - resourceSizeStats[i][RES_COMPRESSED]) / (float)resourceSizeStats[i][RES_SIZE] * 100.0f);

	debug(DBG_RES,"Note: Damn you sound compression rate!");

	debug(DBG_RES,"\nTotal bank files:              %d",resourceUnitStats[STATS_TOTAL_SIZE][RES_SIZE]+resourceUnitStats[STATS_TOTAL_SIZE][RES_COMPRESSED]);
	for(int i=0 ; i < 6 ; i++)
		debug(DBG_RES,"Total %-17s files: %3d",resTypeToString(i),resourceUnitStats[i][RES_SIZE]+resourceUnitStats[i][RES_COMPRESSED]);
#endif
}

/*
	Go over every resource and check if they are marked at "MEMENTRY_STATE_LOAD_ME".
	Load them in memory and mark them are MEMENTRY_STATE_LOADED
*/
void Resource::loadMarkedAsNeeded() {
#ifndef USE_DUMP
  // std::cerr << "    Resource::loadMarkedAsNeeded [in]\n";

	while (1) {

		MemEntry *me = NULL;

		// get resource with max rankNum
		uint8_t maxNum = 0;
		uint16_t i = _numMemList;
		MemEntry *it = _memList;
		while (i--) {
			if (it->state == MEMENTRY_STATE_LOAD_ME && maxNum <= it->rankNum) {
				maxNum = it->rankNum;
				me = it;
			}
			it++;
		}

		if (me == NULL) {
			break; // no entry found
		}


		// At this point the resource descriptor should be pointed to "me"
		// "That's what she said"

		uint8_t *loadDestination = NULL;
		if (me->type == RT_POLY_ANIM) {
			// std::cerr << "    Resource::loadMarkedAsNeeded RT_POLY_ANIM ############################### \n";
      // std::cout << "-----> loading bitmap? " << ((size_t)me - (size_t)_memList)/sizeof(MemEntry) << ' ';
      // std::cout << "size: " << me->size << " bytes.\n";
      // decode background into a byte image
      uint8_t pixels[32000];
      readBank(me, pixels);
      if (numstored_framebuffers == 4) {
        std::cerr << "[fatal] Too many pre-computed framebffers?!\n";
        exit (-1);
      }
      uint8_t *image = framebuffers + 64000*(numstored_framebuffers++);
      {
      for (int j=0;j<200;++j) {
        for (int i=0;i<320;i++) {
          uint8_t idx = 0;
          for (int plane = 3; plane >=0 ; --plane) {
            uint8_t by = pixels[plane*8000 + ((i+j*320)>>3)];
            idx = (idx<<1) | ((by>>(7-(i&7))) & 1);
          }
          image[(i+j*320)] = (idx) | 16;
        }
      }
      }
#if 0
      // image output for debug
      static int cnt = 0;
      SDL_Surface *srf = SDL_CreateRGBSurfaceWithFormat(0,320,200,24, SDL_PIXELFORMAT_RGB24);
      for (int n=0;n<64000;++n) { for (int c=0;c<3;++c) { ((uint8_t*)srf->pixels)[n*3+c] = image[n]; } }
      SDL_SaveBMP(srf,(std::string("test") + std::to_string(cnt++) + ".bmp").c_str());
      SDL_FreeSurface(srf);
#endif
			loadDestination = _vidCurPtr;
		} else {
			loadDestination = _scriptCurPtr;
      // std::cerr << "    Resource::loadMarkedAsNeeded @" << std::hex << (unsigned long long)(_scriptCurPtr-_memPtrStart) << std::dec << "\n";
			if (me->size > _vidBakPtr - _scriptCurPtr) {
				warning("Resource::load() not enough memory");
				me->state = MEMENTRY_STATE_NOT_NEEDED;
				continue;
			}
		}

		if (me->bankId == 0) {
			warning("Resource::load() ec=0x%X (me->bankId == 0)", 0xF00);
			me->state = MEMENTRY_STATE_NOT_NEEDED;
		} else {
			debug(DBG_BANK, "Resource::load() bufPos=%X size=%X type=%X pos=%X bankId=%X", loadDestination - _memPtrStart, me->packedSize, me->type, me->bankOffset, me->bankId);
			readBank(me, loadDestination);
			if (me->type == RT_POLY_ANIM) {
        video->copyPage(_vidCurPtr);
				me->state = MEMENTRY_STATE_NOT_NEEDED;
			} else {
        if (me->type == RT_SOUND) {
          //std::cout << "-----> loading sound id = " << ((size_t)me - (size_t)_memList)/sizeof(MemEntry) << ' ';
          //std::cout << "size: " << me->size << " bytes.\n";
        }
				me->bufPtr = loadDestination;
				me->state  = MEMENTRY_STATE_LOADED;
				_scriptCurPtr += me->size;
			}
		}

	}

  // std::cerr << "    Resource::loadMarkedAsNeeded [out]\n";
#endif
}

void Resource::invalidateRes() {
#ifndef USE_DUMP
	MemEntry *me = _memList;
	uint16_t i = _numMemList;
	while (i--) {
		if (me->type <= RT_POLY_ANIM || me->type > 6) {  // 6 WTF ?!?! ResType goes up to 5 !!
			me->state = MEMENTRY_STATE_NOT_NEEDED;
		}
		++me;
	}
	_scriptCurPtr = _scriptBakPtr;
#endif
}

void Resource::invalidateAll() {
#ifndef USE_DUMP
	MemEntry *me = _memList;
	uint16_t i = _numMemList;
	while (i--) {
		me->state = MEMENTRY_STATE_NOT_NEEDED;
		++me;
	}
	_scriptCurPtr = _memPtrStart;
#endif
}

// --------------------------------------------------------------------------
// SL: Pre-render:
//  - strings as bitmaps to simplify hardware renderer,
//  - pre-rendered framebuffer from the engine.
// --------------------------------------------------------------------------
void Resource::preRenderBuffers() {

  std::vector<unsigned short>        table;
  std::vector<std::vector<uint8_t> > buffers;
  int table_bytes         = (END_OF_STRING_DICTIONARY + 2 + numstored_framebuffers) * 2 /*short*/;
  int table_size_in_lines = (table_bytes / 320) + 1;
  table_bytes             = table_size_in_lines * 320;
  table  .resize(table_bytes>>1, 0);
  if (table.size() < END_OF_STRING_DICTIONARY + 1) {
      std::cerr << "[pre-renderer] improper table size\n";
      exit (-1);
  }
  buffers.resize(END_OF_STRING_DICTIONARY + 1 + numstored_framebuffers);
  int next = table_size_in_lines;
  // reorder
  std::vector<const StrEntry *> entries;
  entries.resize(END_OF_STRING_DICTIONARY + 1, nullptr);
  for (const StrEntry *se = Video::_stringsTableEng; se->id != END_OF_STRING_DICTIONARY; ++se) {
    entries[se->id] = se;
  }
  int max_id = -1;
  for (int s=0;s<entries.size();++s) {
    const StrEntry *se = entries[s];
    if (se == nullptr) continue;
    if (se->id != s) {
      std::cerr << "[pre-renderer] index error\n";
      exit (-1);
    }
    // determine buffer height (in pixels)
    int h = 8; // font is 8 pixels tall
    int len = strlen(se->str);
    for (int i = 0; i < len; ++i) { if (se->str[i] == '\n') { h += 8; } }
    // allocate buffer
    buffers[se->id].resize( h * 320 /*full row*/, 0 );
    // add to table
    if (next > 65535) {
      std::cerr << "[pre-renderer] index too large\n";
      exit (-1);
    }
    max_id          = se->id > max_id ? se->id : max_id;
    table[se->id]   = next;
    next += h;
    table[se->id+1] = next; // ensures last is set
    // debug(DBG_VM, "pre-rendered text  buffer %d from %x to %x",se->id,table[se->id],table[se->id+1]);
    // draw string
    std::vector<uint8_t> draw;
    draw.resize( h * 160 /*4bpp, full row*/, 0 );
    uint16_t x = 0, y = 0;
    for (int i = 0; i < len; ++i) {
      if (se->str[i] == '\n') {
        y += 8;
        x = 0;
        continue;
      }
      video->drawChar(se->str[i], x, y, 15, &draw[0]);
      x++;
    }
    // convert in byte buffer
    for (int j=0;j<h;++j) {
      for (int i=0;i<160;++i) {
        buffers[se->id][(i<<1)+0+j*320] =  (draw[i+j*160] & 0xf0) ? (16 | 0xff) : 0;
        buffers[se->id][(i<<1)+1+j*320] =  (draw[i+j*160] & 0x0f) ? (16 | 0xff) : 0;
      }
    }
  }

  // add framebuffers
  for (int f=0;f<numstored_framebuffers;++f) {
    int id = max_id + 1 + f;
    framebuffer_ids[f] = id;
    table[id]   = next;
    next += 200;
    if (next > 65535) {
      std::cerr << "[pre-renderer] index too large\n";
      exit (-1);
    }
    table[id+1] = next; // ensures last is set
    debug(DBG_VM, "pre-rendered frame buffer %d from %x to %x",id,table[id],table[id+1]);
    // allocate buffer
    buffers[id].resize( 320 *200 , 0 );
    // copy data
    memcpy(&buffers[id][0],framebuffers + f*64000,64000);
  }

  table.back() = next; // close the table
  // debug(DBG_VM, "total pre-render (table+data): %d",next*320 + sizeof(short)*table.size());

#if 0
    // image output for debug
    for (int b = 0; b <buffers.size(); ++b) {
      if (buffers[b].empty()) continue;
      int h = buffers[b].size()/320;
      SDL_Surface *srf = SDL_CreateRGBSurfaceWithFormat(0,320,h,24, SDL_PIXELFORMAT_RGB24);
      uint8_t *ptr = (uint8_t *)srf->pixels;
      for (int j=0;j<h;++j) {
        for (int i=0;i<320;++i) {
          ptr[(i+j*320)*3+0] = buffers[b][i+j*320];
          ptr[(i+j*320)*3+1] = buffers[b][i+j*320];
          ptr[(i+j*320)*3+2] = buffers[b][i+j*320];
        }
      }
      SDL_SaveBMP(srf,(std::string("buffer") + std::to_string(b) + ".bmp").c_str());
      SDL_FreeSurface(srf);
    }
#endif

  // dump file
  FILE *f = fopen("stringtable.raw","wb");
  int check_written = 0;
  check_written += fwrite(&table[0],1,table.size()*sizeof(short),f);
  for (int i=0;i<buffers.size();++i) {
    if (!buffers[i].empty()) {
      // debug(DBG_VM, "[writing] %d at %x (%d bytes)",i,check_written,buffers[i].size());
      check_written += fwrite(&(buffers[i][0]),1,buffers[i].size(),f);
    }
  }
  fclose(f);

}

/* This method serves two purpose:
    - Load parts in memory segments (palette,code,video1,video2)
	           or
    - Load a resource in memory

	This is decided based on the resourceId. If it does not match a mementry id it is supposed to
	be a part id. */
void Resource::loadPartsOrMemoryEntry(uint16_t resourceId) {

#ifndef USE_DUMP
  auto prev = _scriptCurPtr;

  // std::cerr << "    Resource::loadPartsOrMemoryEntry [in] " << std::hex << (unsigned long long)(_scriptCurPtr-_memPtrStart) << std::dec << "\n";

	if (resourceId > _numMemList) {

		requestedNextPart = resourceId;
    // std::cerr << "    Resource::loadPartsOrMemoryEntry [requestedNextPart]\n";

	} else {

		MemEntry *me = &_memList[resourceId];

		if (me->state == MEMENTRY_STATE_NOT_NEEDED) {
			me->state = MEMENTRY_STATE_LOAD_ME;
			loadMarkedAsNeeded();
		}

	}
}

void Resource::dumpDataPack()
{
  // ---------------------------------------------------
  // dump loaded content in a file (latest contains all)
  // ---------------------------------------------------
  /*if (_scriptCurPtr > prev)*/ {
    FILE *dump = NULL;
  	// dump raw data
    dump = fopen("data.raw","wb");
    if (dump) {

      size_t sz_written  = 0;

#if 0
      uint32_t step_over = 4*sizeof(uint32_t); // to step over the 4 offsets we add at the beginning
      const uint32_t offs_max = 1<<17;

      uint32_t offs = (uint32_t)(segBytecode - _memPtrStart) + step_over;
      if (offs > offs_max) { std::cerr << "ERROR: offset exceeds max!\n" << offs << '\n'; exit (-1); }
      sz_written += fwrite(&offs,1,sizeof(offs),dump);

      offs = (uint32_t)(segPalettes - _memPtrStart) + step_over;
      if (offs > offs_max) { std::cerr << "ERROR: offset exceeds max!\n" << offs << '\n'; exit (-1); }
      sz_written += fwrite(&offs,1,sizeof(offs),dump);

      offs = (uint32_t)(segCinematic - _memPtrStart) + step_over;
      if (offs > offs_max) { std::cerr << "ERROR: offset exceeds max!\n" << offs << '\n'; exit (-1); }
      sz_written += fwrite(&offs,1,sizeof(offs),dump);

      offs = (uint32_t)(_segVideo2 - _memPtrStart) + step_over;
      if (_segVideo2 && offs > offs_max) { std::cerr << "ERROR: offset exceeds max!\n" << offs << '\n'; exit (-1); }
      sz_written += fwrite(&offs,1,sizeof(offs),dump);
#endif

      sz_written += fwrite(_memPtrStart,1,_scriptCurPtr-_memPtrStart,dump);

      if (sz_written > (1<<20)) {
        std::cerr << "ERROR: data package exceeds 1MB!\n";
        exit (-1);
      }

      // pad 1MB
      uint8_t zero = 0;
      while (sz_written < (1<<20)) {
        size_t n = fwrite(&zero,1,1,dump);
        if (n == 0) {
          std::cerr << "ERROR: write error while packaging data!\n";
          exit (-1);
        }
        sz_written += n;
      }

      // append pre-rendered buffers
      {
        FILE *strs = NULL;
        strs = fopen("stringtable.raw","rb");
        if (strs == NULL) {
          std::cerr << "ERROR: cannot find strings table file (stringtable.raw)!\n";
          exit (-1);
        }
        while (1) { // copy (yes, this is ugly and slow, but simple)
          uint8_t by;
          size_t n = fread(&by,1,1,strs);
          if (n == 0) break;
          fwrite(&by,1,1,dump);
        }
        fclose(strs);
      }

      fclose(dump);

      // output data.si
      {
        FILE *si = fopen("data.si","w");
        if (si) {
          unsigned int offs = (unsigned int)(segBytecode - _memPtrStart);
              fprintf(si,"$$segBytecode = %d\n",offs);
          offs = segPalettes - _memPtrStart;
              fprintf(si,"$$segPalettes = %d\n",offs);
          offs = segCinematic - _memPtrStart;
              fprintf(si,"$$segCinematic = %d\n",offs);
          offs = _segVideo2 - _memPtrStart;
              fprintf(si,"$$segVideo2 = %d\n",offs);
          fclose(si);
        }
      }

    }

  }
  // std::cerr << "    Resource::loadPartsOrMemoryEntry [out] " << std::hex << (unsigned long long)(_scriptCurPtr-_memPtrStart) << std::dec << "\n";
#endif

}


void Resource::loadDump(const char *f)
{
	// std::cerr << "    Resource::loadDump [in]\n";
  FILE *dump = NULL;
  dump = fopen(f,"rb");
  if (dump) {
    auto nbytes = fread(_memPtrStart,1,MEM_BLOCK_SIZE,dump);
    // std::cerr << "read " << nbytes << '\n';
    _scriptCurPtr = _memPtrStart + nbytes - sizeof(unsigned long long)*4;
    segBytecode   = _memPtrStart + *(unsigned long long*)(_memPtrStart + nbytes - sizeof(unsigned long long)*4);
    segPalettes   = _memPtrStart + *(unsigned long long*)(_memPtrStart + nbytes - sizeof(unsigned long long)*3);
    segCinematic  = _memPtrStart + *(unsigned long long*)(_memPtrStart + nbytes - sizeof(unsigned long long)*2);
    _segVideo2    = _memPtrStart + *(unsigned long long*)(_memPtrStart + nbytes - sizeof(unsigned long long)*1);
    fclose(dump);
  }
	// std::cerr << "    Resource::loadDump [out] "  << std::hex << (unsigned long long)(_scriptCurPtr-_memPtrStart) << std::dec << "\n";
}


/* Protection screen and cinematic don't need the player and enemies polygon data
   so _memList[video2Index] is never loaded for those parts of the game. When
   needed (for action phrases) _memList[video2Index] is always loaded with 0x11
   (as seen in memListParts). */
void Resource::setupPart(uint16_t partId) {
#ifndef USE_DUMP
	if (partId == currentPartId)
		return;

  // std::cerr << "    Resource::setupPart [in]\n";

	if (partId < GAME_PART_FIRST || partId > GAME_PART_LAST)
		error("Resource::setupPart() ec=0x%X invalid partId", partId);

	uint16_t memListPartIndex = partId - GAME_PART_FIRST;

	uint8_t paletteIndex = memListParts[memListPartIndex][MEMLIST_PART_PALETTE];
	uint8_t codeIndex    = memListParts[memListPartIndex][MEMLIST_PART_CODE];
	uint8_t videoCinematicIndex  = memListParts[memListPartIndex][MEMLIST_PART_POLY_CINEMATIC];
	uint8_t video2Index  = memListParts[memListPartIndex][MEMLIST_PART_VIDEO2];

	// Mark all resources as located on harddrive.
	invalidateAll();

	_memList[paletteIndex].state = MEMENTRY_STATE_LOAD_ME;
	_memList[codeIndex].state = MEMENTRY_STATE_LOAD_ME;
	_memList[videoCinematicIndex].state = MEMENTRY_STATE_LOAD_ME;

	// This is probably a cinematic or a non interactive part of the game.
	// Player and enemy polygons are not needed.
	if (video2Index != MEMLIST_PART_NONE)
		_memList[video2Index].state = MEMENTRY_STATE_LOAD_ME;


	loadMarkedAsNeeded();

	segPalettes  = _memList[paletteIndex].bufPtr;
	segBytecode  = _memList[codeIndex].bufPtr;
	segCinematic = _memList[videoCinematicIndex].bufPtr;

	// This is probably a cinematic or a non interactive part of the game.
	// Player and enemy polygons are not needed.
	if (video2Index != MEMLIST_PART_NONE)
		_segVideo2 = _memList[video2Index].bufPtr;

	debug(DBG_RES,"");
	debug(DBG_RES,"setupPart(%d)",partId-GAME_PART_FIRST);
	debug(DBG_RES,"Loaded resource %d (%s) in segPalettes.",paletteIndex,resTypeToString(_memList[paletteIndex].type));
	debug(DBG_RES,"Loaded resource %d (%s) in segBytecode.",codeIndex,resTypeToString(_memList[codeIndex].type));
	debug(DBG_RES,"Loaded resource %d (%s) in segCinematic.",videoCinematicIndex,resTypeToString(_memList[videoCinematicIndex].type));

	if (video2Index != MEMLIST_PART_NONE)
		debug(DBG_RES,"Loaded resource %d (%s) in _segVideo2.",video2Index,resTypeToString(_memList[video2Index].type));


	currentPartId = partId;

	// _scriptCurPtr is changed in this->load();
	_scriptBakPtr = _scriptCurPtr;

#else

  loadDump("dump.raw");

#endif

  // std::cerr << "    Resource::setupPart [out]\n";

}

void Resource::allocMemBlock() {
	_memPtrStart = (uint8_t *)malloc(MEM_BLOCK_SIZE);
	_scriptBakPtr = _scriptCurPtr = _memPtrStart;
	_vidBakPtr = _vidCurPtr = _memPtrStart + MEM_BLOCK_SIZE - 0x800 * 16; //0x800 = 2048, so we have 32KB free for vidBack and vidCur
	_useSegVideo2 = false;
}

void Resource::freeMemBlock() {
	free(_memPtrStart);
}

void Resource::saveOrLoad(Serializer &ser) {
#ifndef USE_DUMP
	uint8_t loadedList[64];
	if (ser._mode == Serializer::SM_SAVE) {
		memset(loadedList, 0, sizeof(loadedList));
		uint8_t *p = loadedList;
		uint8_t *q = _memPtrStart;
		while (1) {
			MemEntry *it = _memList;
			MemEntry *me = 0;
			uint16_t num = _numMemList;
			while (num--) {
				if (it->state == MEMENTRY_STATE_LOADED && it->bufPtr == q) {
					me = it;
				}
				++it;
			}
			if (me == 0) {
				break;
			} else {
				assert(p < loadedList + 64);
				*p++ = me - _memList;
				q += me->size;
			}
		}
	}

	Serializer::Entry entries[] = {
		SE_ARRAY(loadedList, 64, Serializer::SES_INT8, VER(1)),
		SE_INT(&currentPartId, Serializer::SES_INT16, VER(1)),
		SE_PTR(&_scriptBakPtr, VER(1)),
		SE_PTR(&_scriptCurPtr, VER(1)),
		SE_PTR(&_vidBakPtr, VER(1)),
		SE_PTR(&_vidCurPtr, VER(1)),
		SE_INT(&_useSegVideo2, Serializer::SES_BOOL, VER(1)),
		SE_PTR(&segPalettes, VER(1)),
		SE_PTR(&segBytecode, VER(1)),
		SE_PTR(&segCinematic, VER(1)),
		SE_PTR(&_segVideo2, VER(1)),
		SE_END()
	};

	ser.saveOrLoadEntries(entries);
	if (ser._mode == Serializer::SM_LOAD) {
		uint8_t *p = loadedList;
		uint8_t *q = _memPtrStart;
		while (*p) {
			MemEntry *me = &_memList[*p++];
			readBank(me, q);
			me->bufPtr = q;
			me->state = MEMENTRY_STATE_LOADED;
			q += me->size;
		}
	}
#endif
}
