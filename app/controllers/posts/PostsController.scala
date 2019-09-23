package controllers.posts

import javax.inject.Inject
import play.api.libs.json.{JsError, JsValue, Json}
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

class PostsController @Inject()(
                                 cc: ControllerComponents,
                                 postRepository: PostRepository,
                                 implicit val executionContext: ExecutionContext
                               ) extends AbstractController(cc) {

  /**
    * This takes a Json in the format of
    *
    * {
    * "id": 1,
    * "title": "My Title",
    * "body": "My Post"
    * }
    *
    * and saves it into the persistance layer. The created Post is then returned.
    *
    * TODO: It should fail, if there is already a Post with the same id present.
    *
    */
  def create(): Action[JsValue] = Action.async(parse.json) { implicit request =>
    val postResult = request.body.validate[Post]
    postResult.fold(
      errors => {
        Future.successful {
          BadRequest(Json.obj("status" -> "400", "message" -> JsError.toJson(errors)))
        }
      },
      post => {
        postRepository.insert(post).map {
          case Right(post) => Created(Json.toJson("status" -> "201", "data" -> post))
          case Left(message) => BadRequest(Json.obj("status" -> "400", "message" -> message))
        }
      }
    )
  }

  /**
    * This returns a Json Array with a list of all Posts.
    *
    * TODO: Should return the Posts in ascending order on the ids.
    */
  def readAll(): Action[AnyContent] = Action.async { implicit request: Request[AnyContent] =>
    postRepository.findAll.map { posts =>
      val json = Json.toJson("status" -> 200, "data" -> posts)
      Ok(json)
    }
  }

  /**
    * TODO: Returns only the post with the matching id
    * TODO: If the post does not exist, returns a 404 with a json like
    *
    * {
    * "status": 404,
    * "message": "Post not found"
    * }
    *
    */
  def readSingle(id: Int): Action[AnyContent] = Action.async { implicit request: Request[AnyContent] =>
    postRepository.find(id).map {
      case Right(post) => Ok(Json.toJson("status" -> "200", "data" -> post))
      case Left(message) => NotFound(Json.obj("status" -> "404", "message" -> message))
    }
  }

  /**
    * Does not contain any body in the request
    *
    * TODO Deletes the post with the given id.
    */
  def delete(id: Int): Action[AnyContent] = Action.async { implicit request: Request[AnyContent] =>
    postRepository.delete(id).map {
      case Right(_) => NoContent
      case Left(message) => NotFound(Json.obj("status" -> "404", "message" -> message))
    }
  }

  /**
    * Request body contains the post.
    *
    * TODO Updates the post with the given id.
    * TODO Changing the id of a post must not possible.
    */
  def update(id: Int): Action[JsValue] = Action.async(parse.json) { implicit request =>
    val putResult = request.body.validate[Post]
    putResult.fold(
      errors => {
        Future.successful {
          BadRequest(Json.obj("status" -> "400", "message" -> JsError.toJson(errors)))
        }
      },
      post => {
        if (post.id != id) {
          Future.successful {
            BadRequest(Json.toJson("status" -> "400", "message" -> "Id of the resource cannot be changed"))
          }
        } else {
          postRepository.update(post).map(persisted => {
            if (persisted._2 == 200) {
              Ok(Json.toJson("status" -> persisted._2.toString, "data" -> persisted._1))
            } else {
              Created(Json.toJson("status" -> persisted._2.toString, "data" -> persisted._1))
            }
          })
        }
      }
    )
  }


}
