package main

import (
  "fmt"
  "math"
  "io/ioutil"
  "log"
  "encoding/json"
)

// Meta is the struct for importing metadata
type Meta struct {
    Title string `json:"title"`
    Avg float64 `json:"avg"`
    Max float64 `json:"max"`
    Wr float64 `json:"wr"`
    BPI100 float64 `json:"BPI100"`
    BPI90 float64 `json:"BPI90"`
    BPI80 float64 `json:"BPI80"`
    BPI70 float64 `json:"BPI70"`
    BPI60 float64 `json:"BPI60"`
    BPI50 float64 `json:"BPI50"`
}

//Matches is the struct for results
type Matches struct {
  Title string
  Result float64
  Case [6]float64
}

func pgf(j float64, m float64) float64{
  if j == m {
    return m
  }
  return 1 + ( j / m - 0.5 ) / ( 1 - j / m )
}

func exec(k float64,z float64,s float64,m float64,_p float64) float64{
  var _s = pgf(s,m)
  var _z = pgf(z,m)
  var _k = pgf(k,m)
  var sPerK = _s / _k
  var zPerK = _z / _k

  var prefix float64 = 100
  var loggedS = math.Log(sPerK)
  var loggedZ = math.Log(zPerK)

  //BPI定義よりs < kの場合は省略

  return prefix * ( math.Pow(loggedS,_p) / math.Pow(loggedZ,_p) )
}


func test(metaData []Meta) []Matches{
    matches := []Matches{}
    results := [6] float64{0,0,0,0,0,0}

    for _, p := range metaData {
      var count int64
      powCoef := 1.175
      sigma := 0.0
      ranks := [6] float64{p.BPI100,p.BPI90,p.BPI80,p.BPI70,p.BPI60,p.BPI50}
      fmt.Printf("\ncalculating:%v\n", p.Title)
      for{
        sigma = 0
        for i := 0; i < 6; i++ {
            sigma = sigma + exec(p.Avg,p.Wr,ranks[i],p.Max,powCoef)
        }
        if(sigma > 450){
          powCoef += 0.000001
        }else if(sigma < 450){
          powCoef -= 0.000001
        }
        if(math.Abs(sigma - 450) < 0.0001 || count == 1000000){
          for i := 0; i < 6; i++ {
            results[i] = exec(p.Avg,p.Wr,ranks[i],p.Max,powCoef)
          }
          if(count == 1000000){
            fmt.Printf("bursted!:%v\n", results)
            //reset
            powCoef = 1.175
          }else{
            fmt.Printf("matched!:%v\n", results)
          }
          fmt.Printf("sigma:%g\npowCoef:%g\n",sigma,powCoef)
          matches = append(matches,Matches{Title:p.Title,Result:powCoef,Case:results})
          break
        }
        count++
      }
    }
    return matches
}


func main(){

  var metaData []Meta

  metaJSON, err := ioutil.ReadFile("meta.json")

  if err != nil {
    log.Fatal(err)
    return
  }

  if err := json.Unmarshal(metaJSON, &metaData); err != nil {
      log.Fatal(err)
  }

  matches := test(metaData)
  fmt.Printf("\nresult:%v\n", matches)
}
