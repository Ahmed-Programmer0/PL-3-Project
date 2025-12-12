module CourseCrud

open Models

// Check if course ID already exists
let courseIdExists (state: SystemState) (id: int) : bool =
    state.Courses |> List.exists (fun c -> c.Id = id)

// Add a new course with manually specified ID
let addCourse (state: SystemState) (course: Course) : OperationResult<SystemState> =
    // Check if ID already exists
    if courseIdExists state course.Id then
        Failure $"Course ID {course.Id} already exists. Please choose a different ID."
    elif course.Id <= 0 then
        Failure "Course ID must be a positive number."
    else
        let newState = { state with Courses = course :: state.Courses }
        Success newState

// Get course by ID
let getCourseById (state: SystemState) (id: int) : Course option =
    state.Courses |> List.tryFind (fun c -> c.Id = id)

// Get course by name
let getCourseByName (state: SystemState) (name: string) : Course option =
    state.Courses 
    |> List.tryFind (fun c -> c.Name.Equals(name, System.StringComparison.OrdinalIgnoreCase))

// Get all courses
let getAllCourses (state: SystemState) : Course list =
    state.Courses

// Update course
let updateCourse (state: SystemState) (id: int) (updatedCourse: Course) : OperationResult<SystemState> =
    match getCourseById state id with
    | Some _ ->
        let updatedCourses = 
            state.Courses 
            |> List.map (fun c -> if c.Id = id then { updatedCourse with Id = id } else c)
        Success { state with Courses = updatedCourses }
    | None ->
        Failure $"Course with ID {id} not found"

// Delete course
let deleteCourse (state: SystemState) (id: int) : OperationResult<SystemState> =
    match getCourseById state id with
    | Some course ->
        // Check if any students have grades for this course
        let hasGrades = 
            state.Students 
            |> List.exists (fun s -> 
                s.Grades |> List.exists (fun g -> g.CourseId = id))
        
        if hasGrades then
            Failure $"Cannot delete course '{course.Name}'. Students have grades for this course."
        else
            let filteredCourses = state.Courses |> List.filter (fun c -> c.Id <> id)
            Success { state with Courses = filteredCourses }
    | None ->
        Failure $"Course with ID {id} not found"

// Search courses by name (partial match)
let searchCoursesByName (state: SystemState) (searchTerm: string) : Course list =
    state.Courses
    |> List.filter (fun c -> c.Name.ToLower().Contains(searchTerm.ToLower()))

// Get count of courses
let getCourseCount (state: SystemState) : int =
    List.length state.Courses

// Check if course name already exists
let courseNameExists (state: SystemState) (name: string) (excludeId: int option) : bool =
    state.Courses
    |> List.exists (fun c -> 
        c.Name.Equals(name, System.StringComparison.OrdinalIgnoreCase) &&
        (match excludeId with
         | Some id -> c.Id <> id
         | None -> true))

// Get all students enrolled in a course (have grades for that course)
let getStudentsInCourse (state: SystemState) (courseId: int) : Student list =
    state.Students
    |> List.filter (fun s -> 
        s.Grades |> List.exists (fun g -> g.CourseId = courseId))

// Get average grade for a specific course across all students
let getCourseAverage (state: SystemState) (courseId: int) : float option =
    let grades = 
        state.Students
        |> List.collect (fun s -> 
            s.Grades 
            |> List.filter (fun g -> g.CourseId = courseId)
            |> List.map (fun g -> g.Score))
    
    if List.isEmpty grades then None
    else Some (List.average grades)

// Get course statistics
let getCourseStatistics (state: SystemState) (courseId: int) : CourseStatistics option =
    match getCourseById state courseId with
    | Some course ->
        let grades = 
            state.Students
            |> List.collect (fun s -> 
                s.Grades 
                |> List.filter (fun g -> g.CourseId = courseId)
                |> List.map (fun g -> g.Score))
        
        if List.isEmpty grades then
            Some {
                Course = course
                TotalStudents = 0
                AverageGrade = None
                HighestGrade = None
                LowestGrade = None
                PassingCount = 0
                FailingCount = 0
            }
        else
            let avg = List.average grades
            let passingCount = grades |> List.filter (fun g -> g >= 60.0) |> List.length
            Some {
                Course = course
                TotalStudents = 
                    state.Students 
                    |> List.filter (fun s -> s.Grades |> List.exists (fun g -> g.CourseId = courseId))
                    |> List.length
                AverageGrade = Some avg
                HighestGrade = Some (List.max grades)
                LowestGrade = Some (List.min grades)
                PassingCount = passingCount
                FailingCount = (List.length grades) - passingCount
            }
    | None -> None

// Get list of all existing course IDs (helpful for admin)
let getAllCourseIds (state: SystemState) : int list =
    state.Courses 
    |> List.map (fun c -> c.Id) 
    |> List.sort

// Suggest next available course ID
let suggestNextCourseId (state: SystemState) : int =
    let existingIds = getAllCourseIds state |> Set.ofList
    let rec findGap currentId =
        if Set.contains currentId existingIds then
            findGap (currentId + 1)
        else
            currentId
    if List.isEmpty state.Courses then 1 else findGap 1