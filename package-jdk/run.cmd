cd %~dp0

set JAVA_HOME=jdk
app\formbuilder\bin\formbuilder.bat

if exist formbuilder (
  echo rd /s /q app jdk run run.cmd > update.cmd
  echo move formbuilder\app . >> update.cmd
  echo move formbuilder\jdk . >> update.cmd
  echo move formbuilder\run . >> update.cmd
  echo move formbuilder\run.cmd . >> update.cmd
  echo rd /s /q formbuilder
  echo run.cmd

  update.cmd
) else (
  del update.cmd
)
