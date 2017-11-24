package com.models

import java.util.UUID

case class Player(id: String = UUID.randomUUID().toString, score: Int = 0)
