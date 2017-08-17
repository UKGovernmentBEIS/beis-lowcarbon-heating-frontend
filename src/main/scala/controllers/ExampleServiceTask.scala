/*
 * Copyright (C) 2016  Department for Business, Energy and Industrial Strategy
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package controllers

import org.activiti.engine.delegate.{DelegateExecution, JavaDelegate}

/**
 * Example service task that does nothing to demonstrate logging, via the `LoggingEventListener`.
 */
class ExampleServiceTask extends JavaDelegate {

  override def execute(execution: DelegateExecution): Unit = {

    println("*********** In ExampleServiceTask Execute ****************")
    // Do nothing
  }
}