module GradeCrud

open Models
open StudentCrud
open GradeCalculation
open CourseCrud

// Add a grade to a student for a specific course
let addGrade (state: SystemState) (studentId: int) (courseId: int) (score: float) : OperationResult<SystemState> =
    if score < 0.0 || score > 100.0 then
        Failure "Grade must be between 0 and 100"
    else
        match getCourseById state courseId with
        | None -> Failure $"Course with ID {courseId} not found"
        | Some _ ->
            match getStudentById state studentId with
            | Some student ->
                let newGrade = createGrade courseId score
                let updatedStudent = addGradeToStudent student newGrade
                updateStudent state studentId updatedStudent
            | None ->
                Failure $"Student with ID {studentId} not found"

// Update a grade at specific index for a student
let updateGrade (state: SystemState) (studentId: int) (gradeIndex: int) (newScore: float) : OperationResult<SystemState> =
    if newScore < 0.0 || newScore > 100.0 then
        Failure "Grade must be between 0 and 100"
    else
        match getStudentById state studentId with
        | Some student ->
            if gradeIndex < 0 || gradeIndex >= List.length student.Grades then
                Failure $"Invalid grade index. Must be between 0 and {List.length student.Grades - 1}"
            else
                let updatedGrades = 
                    student.Grades 
                    |> List.mapi (fun i g -> if i = gradeIndex then { g with Score = newScore } else g)
                let updatedStudent = { student with Grades = updatedGrades }
                updateStudent state studentId updatedStudent
        | None ->
            Failure $"Student with ID {studentId} not found"

// Delete a grade at specific index for a student
let deleteGrade (state: SystemState) (studentId: int) (gradeIndex: int) : OperationResult<SystemState> =
    match getStudentById state studentId with
    | Some student ->
        if List.isEmpty student.Grades then
            Failure "Student has no grades to delete"
        elif gradeIndex < 0 || gradeIndex >= List.length student.Grades then
            Failure $"Invalid grade index. Must be between 0 and {List.length student.Grades - 1}"
        else
            let updatedGrades = 
                student.Grades 
                |> List.mapi (fun i g -> (i, g))
                |> List.filter (fun (i, _) -> i <> gradeIndex)
                |> List.map snd
            let updatedStudent = { student with Grades = updatedGrades }
            updateStudent state studentId updatedStudent
    | None ->
        Failure $"Student with ID {studentId} not found"

// Get all grades for a student
let getStudentGrades (state: SystemState) (studentId: int) : OperationResult<Grade list> =
    match getStudentById state studentId with
    | Some student -> Success student.Grades
    | None -> Failure $"Student with ID {studentId} not found"

// Get all grades for a student in a specific course
let getStudentCourseGrades (state: SystemState) (studentId: int) (courseId: int) : OperationResult<Grade list> =
    match getStudentById state studentId with
    | Some student -> 
        let courseGrades = student.Grades |> List.filter (fun g -> g.CourseId = courseId)
        Success courseGrades
    | None -> Failure $"Student with ID {studentId} not found"

// Get grade with course name for display
let getGradeWithCourseName (state: SystemState) (grade: Grade) : string =
    match getCourseById state grade.CourseId with
    | Some course -> $"{course.Name} ({course.Code}): {grade.Score:F2}"
    | None -> $"Unknown Course (ID: {grade.CourseId}): {grade.Score:F2}"