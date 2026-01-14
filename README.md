# event_planifier_PD
Un proyecto de programacion declarativa de un planificador de eventos hecho en prolog.
Para correr el proyecto se recomienda usando la terminal, con Swi-prolog instalado, usar el comando de "swipl main.pl" y luego en la terminal de swi escribir "main.".

El proyecto tiene una lista de eventos guardada en el archivo eventos.txt y el usuario es capaz de ver los eventos listados y agregar nuevos eventos con nombre, fecha, recursos especificos y duracion.
Los eventos se agregaran unicamente si no hay un evento con ese nombre en esa misma fecha, si los recursos necesarios para el desarrollo del evento se encuentran disponibles en esas fecha y si estos mismos recursos pertenecen al inventario general.
Tiene un sistema de recursos donde el inventario tiene cantidades para cada recurso y asi poder usar un mismo recurso en varios eventos a la vez. 
Se puede agregar un evento en la proximo fecha disponible automaticamente en caso que al usuario no le importe la fecha.

