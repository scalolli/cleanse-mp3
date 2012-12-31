import com.mpatric.mp3agic.{ID3v1Tag, Mp3File}
import java.io._
import org.apache.commons.io.FileUtils

object Hello {


  def main(args: Array[String]) {
//    songsDir.listFiles().filter(_.isDirectory).foreach(album => {
//      println("Original album name is: " + album.getAbsolutePath)
//      if(!album.listFiles().filter(_.isDirectory).isEmpty && album.listFiles().filter(_.isDirectory)(0).isDirectory) {
//        FileUtils.moveDirectoryToDirectory(album.listFiles()(0), album.getParentFile, true)
//      }
////      val newName = (albumName findFirstIn album.getName).get.trim.toLowerCase.split(" ").foldLeft("")((x:String,y) => x + " " + y.capitalize)
////      FileUtils.moveDirectoryToDirectory(album, new File(album.getParentFile, newName), true)
//    })

//    println("Album name is: " + (albumName findFirstIn "Double Dhamaal [2011-MP3-VBR] - [DJLUV]").get.trim)
    trait Function
    case class ShellFunction(name:String) extends Function
    case class ScalaFunction(name:String) extends Function
    val l = List(None, Some(ShellFunction("build")), None)
    l.map(x => x.flatMap(y => Some(ScalaFunction(y.name))))

    def formulateSongName(song : String) : String = {
      val songNumber = """[0-9]+ -""".r
      val songName = """([a-z|A-Z][ |a-z|A-Z|0-9|(|)]+)""".r
      val no = (songNumber findFirstIn song).get
      val name = (songName findFirstIn song.substring(song.lastIndexOf("- ")+1, song.lastIndexOf(".mp3"))).get
      (no.trim + " " + name.trim + ".mp3")
    }

    def renameAlbums(source : String) {
      val albumName = """(([a-z|A-Z|(0-9)+])[ |a-z|A-Z|0-9]+)""".r
      val songsDir = new File(source)
      songsDir.listFiles().filter(_.isDirectory).foreach(album => {
        val newName = (albumName findFirstIn album.getName).get.trim.toLowerCase.split(" ").foldLeft("")((x: String, y) => x + " " + y.capitalize).trim
        if (!album.getName.equalsIgnoreCase(newName)) {
          println("Album : " + album.getName + " renamed to: " + newName)
          album.renameTo(new File(album.getParentFile, newName))
        }
      })
    }


    def iterateAllAlbums(source : File) {
      println("Album: " + source.getName + " started...")
      val destination = new File(FileUtils.getTempDirectoryPath, source.getName)
      FileUtils.forceMkdir(destination)
      replaceIdTags(source, destination, formulateSongName)
      FileUtils.cleanDirectory(source)
      destination.listFiles().foreach(song => FileUtils.copyFileToDirectory(song, source))
//      source.listFiles().foreach(song => {
//        val mp3file = new Mp3File(song.getAbsolutePath)
//        println("Comment: " + mp3file.getId3v1Tag.getComment)
//        println("Title: " + mp3file.getId3v1Tag.getTitle)
//        println("Album: " + mp3file.getId3v1Tag.getAlbum)
//        println("Artist: " + mp3file.getId3v1Tag.getArtist)
//        println("Year: " + mp3file.getId3v1Tag.getYear)
//        println("Genre: " + mp3file.getId3v1Tag.getGenre)
//      })
      FileUtils.forceDelete(destination)
      println("Album: " + source.getName + " done.")
    }

    def replaceIdTags(source: File, destination: File, f: (String) => String) {
      source.listFiles().filter(!_.isDirectory).filter(_.getName.endsWith(".mp3")).foreach(song => {
        val mp3file = new Mp3File(song.getAbsolutePath)
        val tag = new ID3v1Tag()
        val songName = f(song.getName)
        tag.setTitle(songName)
        tag.setAlbum(source.getName)

        val patternTag = mp3file match {
          case x if x.hasId3v1Tag => Some(x.getId3v1Tag)
          case y if y.hasId3v2Tag => Some(y.getId3v2Tag)
          case _ => None
        }

        patternTag match {
          case Some(x) => {
            tag.setArtist(x.getArtist)
            tag.setYear(x.getYear)
            tag.setTrack(x.getTrack)
          }
          case _ =>
        }

//        if (mp3file.hasId3v1Tag) {
//          tag.setArtist(mp3file.getId3v1Tag.getArtist)
//          tag.setYear(mp3file.getId3v1Tag.getYear)
//          tag.setTrack(mp3file.getId3v1Tag.getTrack)
//        } else if (mp3file.hasId3v2Tag) {
//          tag.setArtist("")
//          tag.setYear(mp3file.getId3v2Tag.getYear)
//          tag.setTrack(mp3file.getId3v2Tag.getTrack)
//        }
        tag.setGenre(24)
        mp3file.removeId3v1Tag()
        mp3file.removeId3v2Tag()
        mp3file.setId3v1Tag(tag)
        val destinationFile = new File(destination.getAbsolutePath + "/" + songName)
        if (destinationFile.exists()) {
          destinationFile.delete()
        }
        mp3file.save(destinationFile.getAbsolutePath)
      })
    }

    val location = "/media/songs/Hindi/process"
//    renameAlbums(location)
    (new File(location)).listFiles().filter(_.isDirectory).foreach(iterateAllAlbums(_))
    println("Done..")
  }


}
