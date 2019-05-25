package futures

import java.awt.Graphics2D

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}
import java.io.File

import javax.imageio.ImageIO
import java.awt.image.BufferedImage

import scala.util.Sorting.quickSort

object medianFilterApp extends App {

  // Val declarations
  val photoFilePath = "out/photo2.jpg"
  val startTime = currentTime
  val photo_in = ImageIO.read(new File(photoFilePath))
  val photo1_out: Future[BufferedImage] = medianFilter(photo_in)
  val photo2_out: Future[BufferedImage] = concurrentMedianFilter(photo_in)

  // Starting the serial implementation of the medianFilter.
  val serialDone: Future[BufferedImage] = for{
    serial<- photo1_out
  } yield serial

  serialDone.onComplete{
    case Success(image) =>{
      ImageIO.write(image, "jpg", new File("serial.jpg"))
      val serialTime = deltaTime(startTime)
      println(s"Finished filtering serially after $serialTime")
    }
    case Failure(e) => e.printStackTrace
  }


  // Starting the concurrent implementation of the median filter.
  val concurrentDone: Future[(BufferedImage)] = for{
    concurrent <- photo2_out
  } yield concurrent

  concurrentDone.onComplete{
    case Success(image: BufferedImage) =>{
      ImageIO.write(image, "jpg", new File("concurrent.jpg"))
      val concurrentTime = deltaTime(startTime)
      println(s"Finished filtering concurrently after $concurrentTime")
    }
    case Failure(e) => e.printStackTrace
  }

  // important for a little parallel demo: need to keep
  // the jvm’s main thread alive
  sleep(10000)
//  test()

  def sleep(time: Long): Unit = Thread.sleep(time)

  // Concurrent median filter code
  def concurrentMedianFilter(img: BufferedImage): Future[BufferedImage]= {
    // Initial variables for readability.
    val w = img.getWidth
    val h = img.getHeight
    val inc =  h/4
    val h0 = 0
    val h1 =  h0+inc
    val h2 = h1+inc
    val h3 = h2+inc

    val out = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB) // <-- Preps a blank image the same size as the original.
    val out_graphics = out.createGraphics() // <-- Uses the Graphics object to write to the out image.

    // Generating submimages for readability.
    val part1 = img.getSubimage(0,h0, w, inc)
    val part2 = img.getSubimage(0,h1, w, inc)
    val part3 = img.getSubimage(0,h2, w, inc)
    val part4 = img.getSubimage(0,h3, w, h-(3*inc))

    // Assigning the median Filter to each sub image for readability.
    val processed1 = medianFilter(part1)
    val processed2 = medianFilter(part2)
    val processed3 = medianFilter(part3)
    val processed4 = medianFilter(part4)

    // Concurrently filtering each part.
    for{
      p1 <- processed1
      p2 <- processed2
      p3 <- processed3
      p4 <- processed4
    } yield putTogether(p1, p2, p3, p4, out_graphics, out) // <-- Write each part to the final image and output it.
  }

  // Helper method to paint the filtered parts together.
  def putTogether(image: BufferedImage, image1: BufferedImage, image2: BufferedImage,
                  image3: BufferedImage, graphic: Graphics2D, output: BufferedImage): BufferedImage = {
    graphic.drawImage(image,0,0,null)
    graphic.drawImage(image1,0,image1.getHeight, null)
    graphic.drawImage(image2,0,image2.getHeight*2, null)
    graphic.drawImage(image3, 0, image2.getHeight*3, null)
    output
  }


  // Base Median Filter code. Does not handle edges.
  def medianFilter(img: BufferedImage): Future[BufferedImage]= Future{
    val startTime = currentTime
    val w = img.getWidth
    val h = img.getHeight
    val out = new BufferedImage(w, h, BufferedImage.TYPE_INT_RGB) // <-- Preps a blank image the same size as the original.


    val kernelwidth = 3
    val kernelheight = 3

    val rMedian:Array[Int] = new Array [Int](kernelwidth*kernelheight)
    val gMedian:Array[Int] = new Array [Int](kernelwidth*kernelheight)
    val bMedian :Array[Int]= new Array [Int](kernelwidth*kernelheight)

    var kerneliter = 0
    var col = 0
    var colfinal = 0

    // Walk the entire image but stop before you go out of bounds at the kernel boundraries.
    for ( i <- 0 until (w-2)){
      for ( j <- 0 until (h-2)){
        // Walk the kernel itself.
        for (ki <-  i until (i + kernelwidth)){
          for(kj <- j until (j + kernelheight)){
             col = img.getRGB(ki, kj)
            rMedian(kerneliter) = (col & 0xff0000) / 65536
            gMedian(kerneliter) = (col & 0xff00) / 256
            bMedian(kerneliter) = (col & 0xff)
            kerneliter += 1
          }
        }
        kerneliter = 0
        quickSort(rMedian)
        quickSort(gMedian)
        quickSort(bMedian)
        colfinal = (rMedian(4)*65536) + (gMedian(4)*256) + bMedian(4)
        out.setRGB(i+1, j+1, colfinal)
      }
    }
    val endTime = deltaTime(startTime)
    System.out.println(s"Finished a medianFilter run after $endTime ")
    out
  }

  def currentTime = System.currentTimeMillis()
  def deltaTime(t0: Long) = currentTime - t0
}