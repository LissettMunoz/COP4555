open System.IO
open System.Text

let generateSamples seconds frequency =
    let sampleRate = 44100.
    let sixteenBitSampleLimit = 32767.
    let numOfSamples = seconds * sampleRate
    let reqSamples = seq{1.0..numOfSamples}
    let volume = 0.8
    let toAmplitude x = 
        x
        |> (*) (2. * System.Math.PI * frequency /sampleRate)
        |> sin
        |> (*) sixteenBitSampleLimit
        |> (*) volume
        |> int16

    Seq.map toAmplitude reqSamples


let pack (d:int16[])=
    let stream = new MemoryStream();
    let writer=new BinaryWriter(stream,System.Text.Encoding.ASCII);
    let dataLength = Array.length d * 2
    //RIFF
    writer.Write(Encoding.ASCII.GetBytes("RIFF"))
    writer.Write(Array.length d)
    writer.Write(Encoding.ASCII.GetBytes("WAVE"))
    writer.Write(Encoding.ASCII.GetBytes("fmt "))
    writer.Write(16)
    writer.Write(1s) //PCM
    writer.Write(1s) //mono
    writer.Write(44100)//rate
    writer.Write(44100 * 16 / 8) //byte rate
    writer.Write(2s)//by per sample
    writer.Write(16s)//bits per sample
    writer.Write(Encoding.ASCII.GetBytes("data"))
    writer.Write(dataLength)
    let data:byte[]=Array.zeroCreate dataLength
    System.Buffer.BlockCopy(d,0,data,0,dataLength)
    writer.Write(data)
    stream

let write fileName (ms:MemoryStream) =
    use fs = new FileStream(Path.Combine(__SOURCE_DIRECTORY__,fileName),FileMode.Create)
    ms.WriteTo(fs)

Array.ofSeq(generateSamples 10. 500.)
        |>pack
        |>write "test.wav"