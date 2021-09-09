const gulp = require('gulp')
const rhtmlBuildUtils = require('rhtmlBuildUtils')

const dontRegisterTheseTasks = []
rhtmlBuildUtils.registerGulpTasks({ 
  gulp: gulp, 
  exclusions: dontRegisterTheseTasks 
})