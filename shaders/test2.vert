vec3 normal_vector = vec3(1.0);
float varying_amount = .5;
int x = 20;

float im_proud_of_parsing_this = 0.1e-5f;

void main(int argc) {
    gl_Position = vec3(     1.0f     ,
      1.0f        , 2.0f
        );
    float n             =   normal_vector;

    float   d         ;

    if (n > 0)
                    d =                          dot(normal_vector,      light_direction);

    else {
        d = -3.0f + 2.0f;
                                    }

              int       z   = n > 0 ?         1 <<         2 % x * 4 : 2;

                    ++z;
}

            int l = 1 && 2 ^^    5 ||           2;
