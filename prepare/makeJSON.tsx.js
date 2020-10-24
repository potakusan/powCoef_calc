//BPIManagerに組み込んでavg,notes,wrを出力

import { songsDB } from "@/components/indexedDB";
import { songData } from "@/types/data";
import { meta } from "./data";

export const exec = async ()=>{
  const songs = (await new songsDB().getAll());
  const songsObj = songs.reduce((groups:{[key:string]:songData},item:songData)=>{
    const title = item.title + "[" + (item.difficulty === "3" ? "H" : item.difficulty === "4" ? "A" : "L") + "]";
    groups[title] = item;
    return groups;
  },{});
  const _data = meta;
  let result:any = [];
  console.log(songsObj);
  for(let i = 0;i < _data.length; ++i){
    const song = songsObj[_data[i]["title"]];
    if(!song){continue}
    result.push(Object.assign(_data[i],{
      "avg":song["avg"],
      "notes":song["notes"],
      "wr":song["wr"],
    }));
  }
  console.log(JSON.stringify(result));
}
