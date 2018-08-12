#include <stdio.h>
#include <assert.h>
#include <SDL2/SDL.h>
#include <SDL2/SDL_image.h>
#include <SDL2/SDL_ttf.h>

#define DEBUG

#ifdef DEBUG
	#define dprint(v) printf("%s = %d\n", #v, v)
#else
	#define  dprint(v)
#endif

typedef struct {
    long length;
    unsigned char chars[1];
} string;

string *SDL_GetError_Tig() {
	const char *msg=SDL_GetError();
	static char buf[512];
	long *len = (long *)buf;
	*(long*)buf = strlen(msg);
	memccpy(buf+sizeof(long), msg, '\0', 512);
	return (string*)&buf;
	
}

long atoi_Tig(string *s){
	return atoi(s->chars);
}

typedef struct {
	long format, w, h, refresh_rate;
	void *driver_data;
} SDL_DisplayMode_Tig;

long SDL_GetDesktopDisplayMode_Tig(const long displayIndex, SDL_DisplayMode_Tig *dmt){
	SDL_DisplayMode dm;
	long ret=SDL_GetDesktopDisplayMode(displayIndex, &dm);
	dmt->format = dm.format;
	dmt->w = dm.w;
	dmt->h = dm.h;
	dmt->refresh_rate = dm.refresh_rate;
	dmt->driver_data = dm.driverdata;
	return ret;
}

SDL_Window *SDL_CreateWindow_Tig(const string *name, const long x, const long y, const long w, const long h, const long flags){
	return SDL_CreateWindow(name->chars, x, y, w, h, flags);
}

SDL_Texture *IMG_LoadTexture_Tig(SDL_Renderer *ren, const string *file){
	return IMG_LoadTexture(ren, file->chars);
}

typedef struct {
	long x,y,w,h;
} SDL_Rect_Tig;

long SDL_BlitSurface_Tig(SDL_Surface *ssrc, const SDL_Rect_Tig *srcrect, SDL_Surface *sdst, const SDL_Rect_Tig *dstrect){
	SDL_Rect *src=NULL, *dst=NULL;
	SDL_Rect srcrect2, dstrect2;
	if(srcrect!=NULL){
		srcrect2.x = srcrect->x;
		srcrect2.y = srcrect->y;
		srcrect2.w = srcrect->w;
		srcrect2.h = srcrect->h;
		src = &srcrect2;
	}
	if(dstrect!=NULL){
		dstrect2.x = dstrect->x;
		dstrect2.y = dstrect->y;
		dstrect2.w = dstrect->w;
		dstrect2.h = dstrect->h;
		dst = &dstrect2;
	}
 	assert(ssrc!=NULL);
 	assert(sdst!=NULL);
	return SDL_BlitSurface(ssrc, src, sdst, dst);
}

long SDL_RenderCopy_Tig(SDL_Renderer *ren, SDL_Texture *tex, const SDL_Rect_Tig *srcrect, const SDL_Rect_Tig *dstrect){
	SDL_Rect *src=NULL, *dst=NULL;
	SDL_Rect srcrect2, dstrect2;
	if(srcrect!=NULL){
		srcrect2.x = srcrect->x;
		srcrect2.y = srcrect->y;
		srcrect2.w = srcrect->w;
		srcrect2.h = srcrect->h;
		src = &srcrect2;
	}
	if(dstrect!=NULL){
		dstrect2.x = dstrect->x;
		dstrect2.y = dstrect->y;
		dstrect2.w = dstrect->w;
		dstrect2.h = dstrect->h;
		dst = &dstrect2;
	}
	return SDL_RenderCopy(ren, tex, src, dst);
}

typedef struct {
	long x,y;
}SDL_Point_Tig;

SDL_Point_Tig *SDL_GetSurfaceSize(const SDL_Surface *surf){
	SDL_Point_Tig *pt = (SDL_Point_Tig*)malloc(sizeof(SDL_Point_Tig));
	pt->x = surf->w;
	pt->y = surf->h;
	return pt;
}

typedef struct {
	long scancode;
	long sym;
	long mod;
} SDL_Keysym_Tig;
typedef struct {
	long timestamp,windowID,state,repeat;
	SDL_Keysym_Tig *keysym;
} SDL_KeyboardEvent_Tig;
typedef struct {
	long tipo;
	SDL_KeyboardEvent_Tig *key;
} SDL_Event_Tig;

string *toString(long a){
	static char buf[64];
	sprintf(buf+sizeof(long), "%d", a);
	*(long*)buf = strlen(buf+sizeof(long));
	return (string*)buf;
}

SDL_Event_Tig *SDL_PollEvent_Tig(){
	SDL_Event event;
	static SDL_Event_Tig event_tig;
	static SDL_KeyboardEvent_Tig event_key_tig;
	static SDL_Keysym_Tig keysym_tig;
	if(!SDL_PollEvent(&event))
		return NULL;
	event_tig.tipo = event.type;
	if(event.type==SDL_KEYDOWN || event.type==SDL_KEYUP){
		event_tig.key = &event_key_tig;
		event_tig.key->keysym = &keysym_tig;
		//~ printf("Se recibe %d keysym %d\n", event.type, event.key.keysym.sym);
		event_tig.key->timestamp = event.key.timestamp;
		event_tig.key->windowID = event.key.windowID;
		event_tig.key->repeat = event.key.repeat;
		event_tig.key->keysym->scancode = event.key.keysym.scancode;
		event_tig.key->keysym->sym = event.key.keysym.sym;
		event_tig.key->keysym->mod = event.key.keysym.mod;
	}
	return &event_tig;
}

TTF_Font *TTF_OpenFont_Tig(const string *file, const long size){
	return TTF_OpenFont(file->chars, size);
}

typedef struct{
	long r,g,b,a;
} SDL_Color_Tig;

SDL_Surface *TTF_RenderText_Blended_Wrapped_Tig(TTF_Font *font, const string *text, const SDL_Color_Tig *color_tig, const long wraplength){
	SDL_Color color;
	color.r = color_tig->r;
	color.g = color_tig->g;
	color.b = color_tig->b;
	color.a = color_tig->a;
	return TTF_RenderText_Blended_Wrapped(font, text->chars, color, wraplength);
}

long SDL_RenderSetLogicalSize_Tig(SDL_Renderer *ren, const long w, const long h){
	return SDL_RenderSetLogicalSize(ren, w, h);
}

long SDL_SetRenderDrawColor_Tig(SDL_Renderer *ren, const long r, const long g, const long b, const long a){
	return SDL_SetRenderDrawColor(ren, r, g, b, a);
}

long SDL_RenderFillRect_Tig(SDL_Renderer *ren, SDL_Rect_Tig *rect_tig){
	SDL_Rect rect, *argrect=NULL;
	if(rect_tig!=NULL){
		rect.x = rect_tig->x;
		rect.y = rect_tig->y;
		rect.w = rect_tig->w;
		rect.h = rect_tig->h;
		argrect = &rect;
	}
	return SDL_RenderFillRect(ren, argrect);
}

void printSDL_Consts(){
	printf("    var SDL_INIT_VIDEO := %d\n", SDL_INIT_VIDEO);
	printf("    var SDL_RENDERER_ACCELERATED := %d\n", SDL_RENDERER_ACCELERATED);
	printf("    var SDL_RENDERER_PRESENTVSYNC := %d\n", SDL_RENDERER_PRESENTVSYNC);
	printf("    var SDL_WINDOW_SHOWN := %d\n", SDL_WINDOW_SHOWN);
	printf("    var SDL_WINDOWPOS_CENTERED := %d\n", SDL_WINDOWPOS_CENTERED);
	printf("    var SDL_FLIP_NONE := %d\n", SDL_FLIP_NONE);
	printf("    var SDL_QUIT := %d\n", SDL_QUIT);
	printf("    var SDL_KEYDOWN := %d\n", SDL_KEYDOWN);
	printf("    var SDLK_ESCAPE := %d\n", SDLK_ESCAPE);
	printf("    var SDLK_LEFT := %d\n", SDLK_LEFT);
	printf("    var SDLK_RIGHT := %d\n", SDLK_RIGHT);
	printf("    var SDLK_UP  := %d\n", SDLK_UP);
	printf("    var SDLK_DOWN := %d\n", SDLK_DOWN);
	printf("    var SDLK_BACKSPACE := %d\n", SDLK_BACKSPACE);
	printf("    var SDLK_DELETE  := %d\n", SDLK_DELETE);
	printf("    var SDLK_r := %d\n", SDLK_r);
	printf("    var SDLK_q := %d\n", SDLK_q);
}
