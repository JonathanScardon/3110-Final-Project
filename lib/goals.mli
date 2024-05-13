val add_new_goal: string -> unit
(**[add_new_goal user] prompts the user for a goal. If it's not already in the list of completed or
    incomplete goals, it gets added to the list of incompleted goals.*)

val log_progress: string -> unit
(**[log_progress user] asks the user to select a goal to update progress on. If the goal is completed or doesn't exist,
    it shows an error. Otherwise, it prompts the user for a progress description and adds it to the goal's log*)

val complete_goal: string -> unit
(**[complete_goal user] prompts the user to choose a goal to mark as completed. If the goal is already been completed
    or doesn't exist, it displays an error message. Otherwise, it moves the goal from the user's incmpleted list 
    to their completed goals.*)


val remove_goal: string -> unit
(**[remove_goal user] prompts the user to select a goal to delete, specifying that only incomplete goals can be removed.
    If the chosen goal is incomplete, it's deleted from their list of incompleted goals along with its progress log*)

val view_incomplete_goals: string -> unit
(**[view_incomplete_goals user] shows the user all current goals in progress. If the user has no in-progress goals,
    it informs the user that no data is availible.*)

val view_complete_goals: string -> unit
(**[view_complete_goals user] shows the user all completed goals. If the user has no completed goals,
    it informs the user that no data is availible.*)

val view_progress_log: string -> unit
(**[view_progress_log user] prompts the user for a goal, completed or in-progress, to view a progres log for. If
    the goal does not exist, it informs the user of this. Otherwise, it displays the data.*)

val delete_goal_logs: string -> unit
(**[delete_goal_logs user] deletes all of the user's goal logs*)