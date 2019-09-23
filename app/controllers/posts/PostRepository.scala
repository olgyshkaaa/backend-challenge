package controllers.posts

import com.google.inject.Inject

import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}

/**
  * A "persistance" layer for the [[Post]]
  */
class PostRepository @Inject()(
                                implicit val executionContext: ExecutionContext
                              ) {

  /**
    * This is the place where all posts are stored. You may change the type, but stick to solution form the
    * scala-std-library.
    */
  private val posts: ListBuffer[Post] = ListBuffer(
    Post(1, "Title 1", "Body 1"),
    Post(2, "Title 2", "Body 2"),
    Post(5, "Title 5", "Body 5"),
    Post(3, "Title 3", "Body 3"),
    Post(4, "Title 4", "Body 4"),
    Post(6, "Title 6", "Body 6"),
    Post(8, "Title 8", "Body 8"),
    Post(9, "Title 9", "Body 9"),
    Post(11, "Title 11", "Body 11")
  )

  def find(id: Int): Future[Either[String, Post]] = {
    Future {
      posts.find(_.id == id) match {
        case Some(post) => Right(post)
        case None => Left("Post not found")
      }
    }
  }

  def findAll: Future[Seq[Post]] = {
    Future {
      posts.sortWith(_.id < _.id)
    }
  }

  def insert(post: Post): Future[Either[String, Post]] = {
    Future {
      posts.find(_.id == post.id) match {
        case Some(_) => Left("Id is already in use")
        case None => posts += post
          Right(post)
      }
    }
  }

  def update(updatedPost: Post): Future[(Post, Int)] = {
    Future {
      val postForUpdate: Option[Post] = posts.find(_.id == updatedPost.id)
      postForUpdate match {
        case Some(post) => posts -= post
          posts += updatedPost
          (updatedPost, 200)
        case None =>
          posts += updatedPost
          (updatedPost, 201)
      }

    }
  }

  def delete(id: Int): Future[Either[String, Int]] = {
    Future {
      val post: Option[Post] = posts.find(_.id == id)
      post match {
        case Some(deletedPost) => posts -= deletedPost
          Right(id)
        case None => Left("Post not found")
      }
    }
  }

}
