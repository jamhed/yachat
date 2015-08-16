require ["conf/requirejs"], (Conf) -> require.config Conf

require ["pi/router"], (Router) -> router = new Router