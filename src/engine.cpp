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

#include "engine.h"
#include "file.h"
#include "serializer.h"
#include "sys.h"
#include "parts.h"

#include <vector>
#include <string>
#include <SDL.h>

Engine::Engine(System *paramSys, const char *dataDir, const char *saveDir)
	: sys(paramSys),
#if 0
  vm(&mixer, &res, &player, &video, sys),
#else
  vm(&res, &video, sys),
#endif
#if 0
  mixer(sys),
	player(&mixer, &res, sys),
#endif
  res(&video, dataDir),
  video(&res, sys), _dataDir(dataDir), _saveDir(saveDir), _stateSlot(0) {
}

void Engine::run() {

	while (!sys->input.quit) {

		vm.checkThreadRequests();

		vm.inp_updatePlayer();

		// processInput();

		vm.hostFrame();
	}


}

Engine::~Engine(){

	finish();
	sys->destroy();
}


void Engine::init(int partId) {


	//Init system
	sys->init("Out Of This World");

	video.init();

	res.allocMemBlock();

#if 1
	res.readEntries();
#endif

	vm.init();

#if 0
	mixer.init();

	player.init();
#endif

	uint16_t part = GAME_PART1 + partId;  // part1 is the protection screen

  vm.initForPart(part);

  // SL: prepare string packages to simplify rendering in hardware
  {
    std::vector<unsigned short> table;
    std::vector<std::vector<uint8_t> > buffers;
    int table_bytes         = (END_OF_STRING_DICTIONARY + 2) * 2 /*short*/;
    int table_size_in_lines = (table_bytes / 320) + 1;
    table_bytes             = table_size_in_lines * 320;
    table  .resize(table_bytes>>1, 0);
    if (table.size() < END_OF_STRING_DICTIONARY + 1) {
        debug(DBG_VM, "improper table size");
        exit (-1);
    }
    buffers.resize(END_OF_STRING_DICTIONARY + 1);
    int next = table_size_in_lines;
    // reorder
    std::vector<const StrEntry *> entries;
    entries.resize(END_OF_STRING_DICTIONARY + 1, nullptr);
    for (const StrEntry *se = Video::_stringsTableEng; se->id != END_OF_STRING_DICTIONARY; ++se) {
      entries[se->id] = se;
    }
    for (int s=0;s<entries.size();++s) {
      const StrEntry *se = entries[s];
      if (se == nullptr) continue;
      if (se->id != s) {
        debug(DBG_VM, "index error");
        exit (-1);
      }
      // determine buffer height
      int h = 8;
      int len = strlen(se->str);
      for (int i = 0; i < len; ++i) { if (se->str[i] == '\n') { h += 8; } }
      // allocate buffer
      buffers[se->id].resize( h * 320 /*full row*/, 0 );
      // add to table
      if (next > 65535) {
        debug(DBG_VM, "index too large");
        exit (-1);
      }
      table[se->id]   = next;
      next += h;
      table[se->id+1] = next;
      debug(DBG_VM, "text %d from %x to %x",se->id,table[se->id],table[se->id+1]);
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
    		video.drawChar(se->str[i], x, y, 15, &draw[0]);
		    x++;
    	}
      // convert in byte buffer
      for (int j=0;j<h;++j) {
        for (int i=0;i<160;++i) {
          buffers[se->id][(i<<1)+0+j*320] =  (draw[i+j*160] & 0xf0) ? 0xff : 0;
          buffers[se->id][(i<<1)+1+j*320] =  (draw[i+j*160] & 0x0f) ? 0xff : 0;
        }
      }
#if 0
      // image output for debug
      SDL_Surface *srf = SDL_CreateRGBSurfaceWithFormat(0,320,h,24, SDL_PIXELFORMAT_RGB24);
      uint8_t *ptr = (uint8_t *)srf->pixels;
      for (int j=0;j<h;++j) {
        for (int i=0;i<320;++i) {
          ptr[(i+j*320)*3+0] = buffers[se->id][i+j*320];
          ptr[(i+j*320)*3+1] = buffers[se->id][i+j*320];
          ptr[(i+j*320)*3+2] = buffers[se->id][i+j*320];
        }
      }
      SDL_SaveBMP(srf,(std::string("test") + std::to_string(se->id) + ".bmp").c_str());
      SDL_FreeSurface(srf);
#endif
    }
    table.back() = next; // close the table
    debug(DBG_VM, "total string table size: %d",next*320 + sizeof(short)*table.size());

    // dump file
    FILE *f = fopen("stringtable.raw","wb");
    int check_written = 0;
    check_written += fwrite(&table[0],1,table.size()*sizeof(short),f);
    for (int i=0;i<buffers.size();++i) {
      if (!buffers[i].empty()) {
        debug(DBG_VM, "[writing] %d at %x (%d bytes)",i,check_written,buffers[i].size());
        check_written += fwrite(&(buffers[i][0]),1,buffers[i].size(),f);
      }
    }
    fclose(f);

    // exit (-1);

  }

  // Try to cheat here. You can jump anywhere but the VM crashes afterward.
	// Starting somewhere is probably not enough, the variables and calls return are probably missing.
	//vm.initForPart(GAME_PART2); // Skip protection screen and go directly to intro
	//vm.initForPart(GAME_PART3); // CRASH
	//vm.initForPart(GAME_PART4); // Start directly in jail but then crash
	//vm.initForPart(GAME_PART5);   //CRASH
	//vm.initForPart(GAME_PART6);   // Start in the battlechar but CRASH afteward
	//vm.initForPart(GAME_PART7); //CRASH
	//vm.initForPart(GAME_PART8); //CRASH
	//vm.initForPart(GAME_PART9); // Green screen not doing anything
}

void Engine::finish() {
#if 0
	player.free();
	mixer.free();
#endif
	res.freeMemBlock();
}

void Engine::processInput() {
	if (sys->input.load) {
		loadGameState(_stateSlot);
		sys->input.load = false;
	}
	if (sys->input.save) {
		saveGameState(_stateSlot, "quicksave");
		sys->input.save = false;
	}
	if (sys->input.stateSlot != 0) {
		int8_t slot = _stateSlot + sys->input.stateSlot;
		if (slot >= 0 && slot < MAX_SAVE_SLOTS) {
			_stateSlot = slot;
			debug(DBG_INFO, "Current game state slot is %d", _stateSlot);
		}
		sys->input.stateSlot = 0;
	}
}

void Engine::makeGameStateName(uint8_t slot, char *buf) {
	sprintf(buf, "raw.s%02d", slot);
}

void Engine::saveGameState(uint8_t slot, const char *desc) {
#if 0
	char stateFile[20];
	makeGameStateName(slot, stateFile);
	File f(true);
	if (!f.open(stateFile, _saveDir, "wb")) {
		warning("Unable to save state file '%s'", stateFile);
	} else {
		// header
		f.writeUint32BE('AWSV');
		f.writeUint16BE(Serializer::CUR_VER);
		f.writeUint16BE(0);
		char hdrdesc[32];
		strncpy(hdrdesc, desc, sizeof(hdrdesc) - 1);
		f.write(hdrdesc, sizeof(hdrdesc));
		// contents
		Serializer s(&f, Serializer::SM_SAVE, res._memPtrStart);
		vm.saveOrLoad(s);
		res.saveOrLoad(s);
		video.saveOrLoad(s);
		player.saveOrLoad(s);
		mixer.saveOrLoad(s);
		if (f.ioErr()) {
			warning("I/O error when saving game state");
		} else {
			debug(DBG_INFO, "Saved state to slot %d", _stateSlot);
		}
	}
#endif
}

void Engine::loadGameState(uint8_t slot) {
#if 0
	char stateFile[20];
	makeGameStateName(slot, stateFile);
	File f(true);
	if (!f.open(stateFile, _saveDir, "rb")) {
		warning("Unable to open state file '%s'", stateFile);
	} else {
		uint32_t id = f.readUint32BE();
		if (id != 'AWSV') {
			warning("Bad savegame format");
		} else {
			// mute
			player.stop();
			mixer.stopAll();
			// header
			uint16_t ver = f.readUint16BE();
			f.readUint16BE();
			char hdrdesc[32];
			f.read(hdrdesc, sizeof(hdrdesc));
			// contents
			Serializer s(&f, Serializer::SM_LOAD, res._memPtrStart, ver);
			vm.saveOrLoad(s);
			res.saveOrLoad(s);
			video.saveOrLoad(s);
			player.saveOrLoad(s);
			mixer.saveOrLoad(s);
		}
		if (f.ioErr()) {
			warning("I/O error when loading game state");
		} else {
			debug(DBG_INFO, "Loaded state from slot %d", _stateSlot);
		}
	}
#endif
}
