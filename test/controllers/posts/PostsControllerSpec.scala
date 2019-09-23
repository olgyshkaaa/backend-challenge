package controllers.posts

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import org.scalatestplus.play._
import org.scalatestplus.play.guice._
import play.api.libs.json.Json
import play.api.test.Helpers.{contentType, status, _}
import play.api.test._

import scala.concurrent.ExecutionContext

class PostsControllerSpec extends PlaySpec with GuiceOneAppPerTest with Injecting {

  lazy val mockPostsRepository: PostRepository = new PostRepository()
  implicit val sys: ActorSystem = ActorSystem("TestActor")
  implicit lazy val executionContext: ExecutionContext = inject[ExecutionContext]
  implicit val mat: ActorMaterializer = ActorMaterializer()


  "PostsController GET" should {

    "return the ordered list of posts from the controller" in {
      val controller = new PostsController(stubControllerComponents(), mockPostsRepository, executionContext)
      val response = controller.readAll().apply(FakeRequest(GET, "/api/v1/posts"))
      val bodyText = contentAsString(response)

      status(response) mustBe OK
      contentType(response) mustBe Some("application/json")
      bodyText.contains("status") mustBe true
      bodyText.contains("200") mustBe true
      bodyText.contains("data") mustBe true
      bodyText.indexOf("\"id\":1") < bodyText.indexOf("\"id\":2") mustBe true
      bodyText.indexOf("\"id\":2") < bodyText.indexOf("\"id\":3") mustBe true
      bodyText.indexOf("\"id\":3") < bodyText.indexOf("\"id\":4") mustBe true
      bodyText.indexOf("\"id\":4") < bodyText.indexOf("\"id\":5") mustBe true

    }

    "return the post with certain id from the controller" in {
      val post = """{"id":6,"title":"Title 6","body":"Body 6"}"""
      val controller = new PostsController(stubControllerComponents(), mockPostsRepository, executionContext)
      val response = controller.readSingle(6).apply(FakeRequest(GET, "/api/v1/posts/6"))
      val bodyText = contentAsString(response)

      status(response) mustBe OK
      contentType(response) mustBe Some("application/json")
      bodyText.contains("status") mustBe true
      bodyText.contains("200") mustBe true
      bodyText.contains("data") mustBe true
      bodyText.contains(post) mustBe true

    }

    "return info about not found post from the controller" in {
      val controller = new PostsController(stubControllerComponents(), mockPostsRepository, executionContext)
      val response = controller.readSingle(1111111).apply(FakeRequest(GET, "/api/v1/posts/1111111"))
      val bodyText = contentAsString(response)

      status(response) mustBe NOT_FOUND
      contentType(response) mustBe Some("application/json")
      bodyText.contains("status") mustBe true
      bodyText.contains("404") mustBe true
      bodyText.contains("message") mustBe true
      bodyText.contains("Post not found") mustBe true
    }
  }

  "PostsController POST" should {

    "return the successfully persisted post from the controller" in {
      val newPost = """{"id":7,"title":"Title 7","body":"Body 7"}"""
      val controller = new PostsController(stubControllerComponents(), mockPostsRepository, executionContext)
      val response = controller.create().apply(FakeRequest(POST, "/api/v1/posts ", FakeHeaders(Seq("Content-type" -> "application/json")), Json.parse(newPost)))
      val bodyText = contentAsString(response)

      status(response) mustBe CREATED
      contentType(response) mustBe Some("application/json")
      bodyText.contains("status") mustBe true
      bodyText.contains("201") mustBe true
      bodyText.contains("data") mustBe true
      bodyText.contains(newPost) mustBe true
    }

    "return info about duplicated id from the controller" in {
      val post = """{"id":8,"title":"Title 8","body":"Body 8"}"""
      val controller = new PostsController(stubControllerComponents(), mockPostsRepository, executionContext)
      val secondResponse = controller.create().apply(FakeRequest(POST, "/api/v1/posts ", FakeHeaders(Seq("Content-type" -> "application/json")), Json.parse(post)))
      val bodyText = contentAsString(secondResponse)

      status(secondResponse) mustBe BAD_REQUEST
      contentType(secondResponse) mustBe Some("application/json")
      bodyText.contains("status") mustBe true
      bodyText.contains("400") mustBe true
      bodyText.contains("message") mustBe true
      bodyText.contains("Id is already in use") mustBe true
    }

    "return info about the incorrect data in body from the controller" in {
      val newPost = """{"invalidData"}"""
      val controller = new PostsController(stubControllerComponents(), mockPostsRepository, executionContext)
      val response = controller.create().apply(FakeRequest(POST, "/api/v1/posts ", FakeHeaders(Seq("Content-type" -> "application/json")), newPost))
      val bodyText = contentAsString(response)

      status(response) mustBe BAD_REQUEST
      contentType(response) mustBe Some("text/html")
      bodyText.contains("Bad Request") mustBe true
      bodyText.contains("Invalid Json") mustBe true
    }
  }

  "PostsController PUT" should {

    "return the successfully updated post from the controller" in {
      val post = """{"id":9,"title":"Title 9 updated","body":"Body 9 updated"}"""
      val controller = new PostsController(stubControllerComponents(), mockPostsRepository, executionContext)
      val response = controller.update(9).apply(FakeRequest(PUT, "/api/v1/posts/9 ", FakeHeaders(Seq("Content-type" -> "application/json")), Json.parse(post)))
      val bodyText = contentAsString(response)

      status(response) mustBe OK
      contentType(response) mustBe Some("application/json")
      bodyText.contains("status") mustBe true
      bodyText.contains("200") mustBe true
      bodyText.contains("data") mustBe true
      bodyText.contains(post) mustBe true
    }

    "return the successfully persisted post from the controller" in {
      val updatedPost = """{"id":10,"title":"Title 10","body":"Body 10"}"""
      val controller = new PostsController(stubControllerComponents(), mockPostsRepository, executionContext)
      val response = controller.update(10).apply(FakeRequest(PUT, "/api/v1/posts/10", FakeHeaders(Seq("Content-type" -> "application/json")), Json.parse(updatedPost)))
      val bodyText = contentAsString(response)

      status(response) mustBe CREATED
      contentType(response) mustBe Some("application/json")
      bodyText.contains("status") mustBe true
      bodyText.contains("201") mustBe true
      bodyText.contains("data") mustBe true
      bodyText.contains(updatedPost) mustBe true
    }

    "return info about the incorrect data in body from the controller" in {
      val newPost = """{"invalidData"}"""
      val controller = new PostsController(stubControllerComponents(), mockPostsRepository, executionContext)
      val response = controller.update(1).apply(FakeRequest(PUT, "/api/v1/posts/1", FakeHeaders(Seq("Content-type" -> "application/json")), newPost))

      val bodyText = contentAsString(response)

      status(response) mustBe BAD_REQUEST
      contentType(response) mustBe Some("text/html")
      bodyText.contains("Bad Request") mustBe true
      bodyText.contains("Invalid Json") mustBe true
    }

    "return info about the different id in body and url from the controller" in {
      val updatedPost = """{"id":10,"title":"Title 10","body":"Body 10"}"""
      val controller = new PostsController(stubControllerComponents(), mockPostsRepository, executionContext)
      val response = controller.update(2).apply(FakeRequest(PUT, "/api/v1/posts/2", FakeHeaders(Seq("Content-type" -> "application/json")), Json.parse(updatedPost)))
      val bodyText = contentAsString(response)

      status(response) mustBe BAD_REQUEST
      contentType(response) mustBe Some("application/json")
      bodyText.contains("status") mustBe true
      bodyText.contains("400") mustBe true
      bodyText.contains("message") mustBe true
      bodyText.contains("Id of the resource cannot be changed") mustBe true
    }
  }

  "PostsController DELETE" should {

    "return info about deleted post from the controller" in {
      val controller = new PostsController(stubControllerComponents(), mockPostsRepository, executionContext)
      val response = controller.delete(11).apply(FakeRequest(DELETE, "/api/v1/posts/11"))

      status(response) mustBe NO_CONTENT
    }

    "return info about not found post from the controller" in {
      val controller = new PostsController(stubControllerComponents(), mockPostsRepository, executionContext)
      val response = controller.delete(1111111).apply(FakeRequest(DELETE, "/api/v1/posts/1111"))
      val bodyText = contentAsString(response)

      status(response) mustBe NOT_FOUND
      contentType(response) mustBe Some("application/json")
      bodyText.contains("status") mustBe true
      bodyText.contains("404") mustBe true
      bodyText.contains("message") mustBe true
      bodyText.contains("Post not found") mustBe true
    }
  }

}

