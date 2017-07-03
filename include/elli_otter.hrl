%%

-record(elli_otter_config, {prefix                    :: binary(),
                            traced_request_attributes :: list(),
                            log_exceptions            :: boolean(),
                            sampling_percent          :: integer(),
                            debug                     :: boolean()}).
